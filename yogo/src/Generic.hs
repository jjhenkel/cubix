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
{-# LANGUAGE UndecidableInstances #-}

module Generic (
  PrimitiveT
  , AddressT
  , ValueT
  , MemoryT
  , MemValT
  , StatementT
  , ScopeT

  , Primitive(..)
  , ConstF(..)
  , UnknownF(..)
  , MemF(..)
  , MemGenesisF(..)

  , Name(..)
  , Occurrence(..)
  , ID(..)
  , Node(..)

  , YGenericSig
  , YGraphEntry(..)
  , YGraph
  , YProject

  , YogoState(..)
  , nameScope
  , memScope
  , file

  , MonadYogo
  , CanYTrans
  , YTranslateM
  , YTrans(..)
  , YFile

  , yinject
  , getScope
  , addEntry
  , getNextID
  , makeNode
  , ytranslate


  ) where

import Data.Proxy ( Proxy(..) )
import Debug.Trace
import Data.Typeable

import Control.Lens ( makeLenses, over, use, (^.), (.=), (%=) )
import Control.Monad ( MonadPlus, mzero, liftM )
import Control.Monad.Trans.Maybe ( runMaybeT )
import Control.Monad.Identity ( Identity(..), runIdentity )
import Control.Monad.State ( MonadState, execStateT, evalStateT, runStateT, modify, get )
import Data.Comp.Multi
import Data.Comp.Multi.Strategic
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Maybe ( fromJust )

import Cubix.Language.Info
import qualified Cubix.Language.Parametric.Syntax as C

-- Types in Yogo
data PrimitiveT
data AddressT
data ValueT
data MemoryT
data MemValT

-- Syntax types, can't be used as node arguments
data StatementT
data ScopeT -- functions, modules

data Primitive = IntegerF Integer | IntF Int | BoolF Bool | StringF [Char]

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
  Statement :: ID f StatementT
  Statements :: ID f [StatementT]
  Scope :: ID f ScopeT

data Node (f :: (* -> *) -> * -> *) t = Node (f (ID f) t)

type YGenericSig = UnknownF :+: ConstF :+: MemF :+: MemGenesisF

data YGraphEntry f = YGraphNode (E (ID f)) (E (Node f)) Occurrence
                   | YGraphEq (E (ID f)) (E (ID f))
                   | YGraphMemSuccessor (ID f MemoryT) (ID f MemoryT)

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

type CanYTrans f = (HFunctor f, ShowHF f, Typeable f)

type YTranslateM m f sig ysig t = GTranslateM m ((f :&: Label) (TermLab sig)) (ID ysig t)

instance Show (ID f t) where
  show (ID n) = "(ID " ++ show n ++ ")"
  show Statement = "Statement"
  show Statements = "[Statement]"
  show Scope = "Scope"

instance Show (YGraphEntry f) where
  show (YGraphNode (E id) _ _) = show id
  show (YGraphEq (E id1) (E id2)) = "Eq " ++ show id1 ++  " " ++ show id2
  show (YGraphMemSuccessor id1 id2) = "MemSuc " ++ show id1 ++ " " ++ show id2

yinject :: (g :<: f) => g (ID f) t -> Node f t
yinject = Node . inj

getScope :: (MonadYogo f m) => m Name
getScope = liftM head $ use nameScope

addEntry :: (MonadYogo f m) => YGraphEntry f -> m ()
addEntry e = do
  name <- getScope
  m <- use file
  traceM $ "Before Add " ++ (show (Map.lookup name m))
  file %= Map.adjust (e :) name
  m <- use file
  traceM $ "After Add " ++ (show (Map.lookup name m))

getNextID :: (MonadYogo f m) => m (ID f t)
getNextID = lastID %= (+ 1) >> liftM ID (use lastID)

makeNode :: (MonadYogo f m, g :<: f) => g (ID f) t -> [Label] -> m (ID f t)
makeNode node labels = do
  id <- getNextID
  addEntry $ YGraphNode (E id) (E $ yinject node) (Occurrence labels)
  return id

ytransUnknown :: (CanYTrans f, MonadYogo y m, UnknownF :<: y) => YTranslateM m f g y MemoryT
ytransUnknown (f :&: label) = do
  mems <- use memScope
  id <- makeNode (UnknownF (head mems)) [label]
  memScope .= id : (tail mems)
  return id

-- s is needed, otherwise cause Incoherent Instances when f is a signature type
class (CanYTrans f) => YTrans f g y t where
  ytrans :: (MonadYogo y m) => YTranslateM m f g y t

ytranslate :: (CanYTrans f, MonadYogo y m, YTrans f f y t) => GTranslateM m (TermLab f) (ID y t)
ytranslate f = (ytrans . unTerm) f

instance {-# OVERLAPPABLE #-} (CanYTrans f) => YTrans f g y t where
  ytrans = const mzero
  -- ytrans = error $ show (typeRep (Proxy :: Proxy f))

instance {-# OVERLAPPING #-} (YTrans f1 g y t, YTrans f2 g y t) => YTrans (f1 :+: f2) g y t where
  ytrans = caseH' ytrans ytrans

instance {-# OVERLAPPABLE #-} (CanYTrans f) => YTrans f g y MemoryT where
  ytrans = const mzero
  -- ytrans = error $ show (typeRep (Proxy :: Proxy f)) ++ "yo"

instance {-# OVERLAPPING #-} (YTrans f1 g y MemoryT, YTrans f2 g y MemoryT) => YTrans (f1 :+: f2) g y MemoryT where
  ytrans = caseH' ytrans ytrans

instance (YTrans g g y StatementT, YTrans g g y [StatementT]) => YTrans C.ListF g y [StatementT] where
  ytrans (C.NilF :&: _) = return Statements
  ytrans ((C.ConsF x xs) :&: _) = do
    _ :: ID y StatementT <- ytranslate x
    _ :: ID y [StatementT] <- ytranslate xs
    return Statements
