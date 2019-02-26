{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}

module Generic (
  YGenericSig,
  YPythonSig,
  YFile,
  YProject,
  toGraphPython,
  ) where

import Control.Lens ( makeLenses, over, use, (^.), (.=), (%=) )
import Control.Monad ( MonadPlus, mzero, liftM )
import Control.Monad.Trans.Maybe ( runMaybeT )
import Control.Monad.Identity ( Identity(..), runIdentity )
import Control.Monad.State ( MonadState, execStateT, evalStateT, runStateT, modify, get )
import Data.Comp.Multi ( AnnTerm, inject', (:&:)(..), HFunctor, unTerm, inj, project', caseH', hmapM, hfmap, Term(..) )
import Data.Comp.Multi.Ops ( (:+:), (:<:) )
import Data.Comp.Multi.HFunctor ( E(..), K(..), (:=>) )
import Data.Comp.Multi.Strategic
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Maybe ( fromJust )
import Cubix.Language.Info
import qualified Cubix.Language.Parametric.Syntax as Cubix
import Cubix.Language.Python.Parametric.Common ( MPythonSig, MPythonTermLab )
import qualified Cubix.Language.Python.Parametric.Common as Py

-- Types in Yogo
data PrimitiveT
data AddressT
data ValueT
data MemoryT
data MemValT

-- Syntax types, can't be used as node arguments
data ScopeT -- functions, modules

data Primitive = IntF Int | BoolF Bool | StringF [Char]

data ConstF (e :: * -> *) t where
  ConstF :: Primitive -> ConstF e ValueT

data UnknownF (e :: * -> *) t where
  UnknownF :: e MemoryT -> UnknownF e MemoryT

data MemF (e :: * -> *) t where
  MemF :: e MemValT -> MemF e MemoryT

data MemGenesisF (e :: * -> *) t where
  MemGenesisF :: MemGenesisF e MemoryT

newtype Name = Name [Char] deriving (Eq, Show, Ord)
newtype Occurrence = Occurrence [Label] deriving (Eq, Show)

data ID (f :: (* -> *) -> * -> *) t where
  ID :: Int -> ID f t
  Scope :: ID f ScopeT

data Node (f :: (* -> *) -> * -> *) t = Node (f (ID f) t)

type YGenericSig = UnknownF :+: ConstF :+: MemF :+: MemGenesisF

data YGraphEntry f = YGraphNode (E (ID f)) (E (Node f)) Occurrence
                   -- | YGraphEq (E ID) (E ID)
                   -- | YGraphMemSuccessor (ID MemoryT) (ID MemoryT)

type YGraph f = [YGraphEntry f]
type YFile f = Map Name (YGraph f)
type YProject f = Map FilePath (YFile f)

data YogoState f = YogoState { _nameScope :: [Name]
                             , _memScope :: [ID f MemoryT]
                             , _file  :: YFile f
                             , _lastID :: Int
                             }
makeLenses ''YogoState

type MonadYogo f m = (MonadPlus m, MonadState (YogoState f) m)

data PyLhsF e l where
  PyLhsF :: e [AddressT] -> PyLhsF e [AddressT]

type YPythonSig = PyLhsF :+: YGenericSig

type YTranslateM m f sig ysig t = GTranslateM m ((f :&: Label) (TermLab sig)) (ID ysig t)

yinject :: (g :<: f) => g (ID f) t -> Node f t
yinject = Node . inj

getScope :: (MonadYogo f m) => m Name
getScope = liftM head $ use nameScope

addEntry :: (MonadYogo f m) => YGraphEntry f -> m ()
addEntry e = getScope >>= \name -> file %= Map.adjust ((:) e) name

getNextID :: (MonadYogo f m) => m (ID f t)
getNextID = lastID %= (+ 1) >> liftM ID (use lastID)

type YTranslatePyM m f t = GTranslateM m ((f :&: Label) MPythonTermLab) (ID YPythonSig t)

ytransUnknown :: (MonadYogo y m, UnknownF :<: y) => YTranslateM m f s y MemoryT
ytransUnknown (f :&: label) = do
  mems <- use memScope
  let mem = yinject $ UnknownF (head mems)
  id <- getNextID
  addEntry $ YGraphNode (E id) (E mem) (Occurrence [label])
  memScope .= id : (tail mems)
  return id

class YTrans f s y t where
  ytrans :: (MonadYogo y m) => YTranslateM m f s y t

instance {-# OVERLAPPABLE #-} YTrans f s y t where
  ytrans = const mzero

instance {-# OVERLAPPABLE #-} (UnknownF :<: y) => YTrans f s y MemoryT where
  ytrans = ytransUnknown

instance {-# OVERLAPPING #-} (YTrans f s y MemoryT, YTrans g s y MemoryT) => YTrans (f :+: g) s y MemoryT where
  ytrans = caseH' ytrans ytrans

instance (YTrans f s y t, YTrans g s y t) => YTrans (f :+: g) s y t where
  ytrans = caseH' ytrans ytrans

instance YTrans Py.Module MPythonSig YPythonSig ScopeT where
  ytrans ((Py.Module body) :&: label) = do
    id <- getNextID
    nameScope %= ((:) $ Name "Module")
    memScope  %= ((:) id)
    (_ :: ID YPythonSig MemoryT) <- ytrans $ unTerm body
    return Scope

ytransPythonModule :: (MonadYogo YPythonSig m) => TranslateM m MPythonTermLab Py.ModuleL (ID YPythonSig ScopeT)
ytransPythonModule t = ytrans $ unTerm t

initState :: YogoState YPythonSig
initState = YogoState [] [] Map.empty 0

fileToGraphPython :: MPythonTermLab l -> YFile YPythonSig
fileToGraphPython t =
  let (a, state) = fromJust $ runIdentity $ runMaybeT $ runStateT ((onetdT $ promoteTF ytransPythonModule) t) initState in
    state ^. file

toGraphPython :: Project MPythonSig -> YProject YPythonSig
toGraphPython = Map.map (\(E t) -> fileToGraphPython t)
-- toGraphPython = error "no"
