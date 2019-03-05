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

module Common.Trans where

import Data.Proxy ( Proxy(..) )
import Debug.Trace
import Data.Typeable

import Control.Lens ( makeLenses, over, use, (.=), (%=) )
import Control.Monad ( MonadPlus, mzero, liftM )
import Control.Monad.Trans.Maybe ( runMaybeT )
import Control.Monad.Identity ( Identity(..), runIdentity )
import Control.Monad.State ( MonadState, execStateT, evalStateT, runStateT, modify, get )
import Data.Comp.Multi
import Data.Comp.Multi.Strategic
import Data.List ( intercalate )
import Data.Map ( Map )
import qualified Data.Map as Map

import Cubix.Language.Info
import qualified Cubix.Language.Parametric.Syntax as Cx

-- Types in Yogo
data PrimitiveT
data AddressT
data ValueT
data MemoryT
data MemValT

-- Syntax types. Do not correspond to any node
data OpT -- Operators
data StatementT
data ScopeT -- functions, modules

data Primitive = IntegerF Integer | IntF Int | BoolF Bool | StringF String

data CommonOp' = And | Or | Not
               | Exponent | Plus | Multiply
               | LessThan | GreaterThan | Equals | GreaterThanEquals | LessThanEquals | NotEquals

-- Engine nodes. Provided by Yogo
data AnyNode (e :: * -> *) t
data AnyLValue (e :: * -> *) t
data AnyMem (e :: * -> *) t

data Q (e :: * -> *) t where
  Q :: e AddressT -> e MemoryT -> Q e ValueT

-- Non-nodes

data CommonOp (e :: * -> *) t where
  CommonOp :: CommonOp' -> CommonOp e OpT

-- Generic nodes
data AnyStack (e :: * -> *) t
data AnyHeap (e :: * -> *) t

data NothingF (e :: * -> *) t where
  NothingF :: NothingF e t

data ConstF (e :: * -> *) t where
  ConstF :: Primitive -> ConstF e ValueT

data IdentF (e :: * -> *) t where
  IdentF :: String -> IdentF e AddressT

data BinOpF (e :: * -> *) t where
  BinOpF :: e OpT -> e ValueT -> e ValueT -> BinOpF e ValueT

-- Arguments by order of execution
data AssignF e t where
  AssignF :: e ValueT -> e AddressT -> e MemoryT -> AssignF e MemValT

data MemGenesisF (e :: * -> *) t where
  MemGenesisF :: MemGenesisF e MemoryT

data MemF e t where
  MemF :: e MemValT -> MemF e MemoryT

data UnknownF e t where
  UnknownF :: e MemoryT -> UnknownF e MemoryT

type YGenericSig = UnknownF :+: MemF :+: MemGenesisF :+: AssignF :+: BinOpF :+: IdentF :+: ConstF :+: NothingF :+: AnyHeap :+: AnyStack :+: CommonOp :+: Q :+: AnyMem :+: AnyLValue :+: AnyNode

newtype Name = Name [Char] deriving (Eq, Show, Ord)
newtype Occurrence = Occurrence [Label] deriving (Eq, Show)

data ID (f :: (* -> *) -> * -> *) t where
  ID :: Int -> ID f t
  IDs :: [(ID f t, Occurrence)] -> ID f [t]
  Statement :: ID f StatementT
  Scope :: ID f ScopeT

instance Show (ID f t) where
  show (ID n) = "(ID " ++ show n ++ ")"
  show (IDs ids) = "[IDS " ++ (intercalate " " $ map show ids) ++ "]"
  show Scope = "Scope"

fromIds :: ID f [t] -> [(ID f t, Occurrence)]
fromIds (IDs ids) = ids
fromIds t = error $ "ID is not a list: " ++ show t

data Node (f :: (* -> *) -> * -> *) t = Node (f (ID f) t)

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

instance Show (YGraphEntry f) where
  show (YGraphNode (E id) _ _) = show id
  show (YGraphEq (E id1) (E id2)) = "Eq " ++ show id1 ++  " " ++ show id2
  show (YGraphMemSuccessor id1 id2) = "MemSuc " ++ show id1 ++ " " ++ show id2

getScopeName :: (MonadYogo f m) => m Name
getScopeName = liftM head $ use nameScope

newScope :: (MonadYogo f m, MemGenesisF :<: f) => Name -> m (ID f MemoryT)
newScope name = do
  nameScope %= (name :)
  file .= Map.singleton name []
  insertNode [] MemGenesisF >>= updateMem

getNextID :: (MonadYogo f m) => m (ID f t)
getNextID = lastID %= (+ 1) >> liftM ID (use lastID)

insertNode' :: (MonadYogo f m, g :<: f) => Occurrence -> g (ID f) t -> m (ID f t)
insertNode' occ node = do
  id <- getNextID
  let entry = YGraphNode (E id) ((E . Node . inj) node) occ
  getScopeName >>= (file %=) . (Map.adjust (entry :))
  return id

insertNode :: (MonadYogo f m, g :<: f) => [Label] -> g (ID f) t -> m (ID f t)
insertNode labels = insertNode' (Occurrence labels)

iMemF :: (MonadYogo f m, MemF :<: f) => [Label] -> ID f MemValT -> m (ID f MemoryT)
iMemF labels memVal = insertNode labels (MemF memVal) >>= updateMem

iQ :: (MonadYogo f m, Q :<: f) => [Label] -> ID f AddressT -> m (ID f ValueT)
iQ labels lvalue = getMem >>= (insertNode labels) . (Q lvalue)

getMem :: (MonadYogo f m) => m (ID f MemoryT)
getMem = liftM head $ use memScope

updateMem :: (MonadYogo f m) => ID f MemoryT -> m (ID f MemoryT)
updateMem mid = memScope %= (mid :) . tail >> return mid

updateMemVal :: (MonadYogo f m, MemF :<: f) => ID f MemValT -> m (ID f MemValT)
updateMemVal memVal = iMemF [] memVal >> return memVal

ytransUnknown :: (CanYTrans f, MonadYogo y m, UnknownF :<: y) => YTranslateM m f g y MemoryT
ytransUnknown (f :&: l) = getMem >>= (insertNode [l]) . UnknownF >>= updateMem

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

-- Not every kinds of ListF needs to become a list in Yogo graph, therefore just
-- return ID f [t] for the caller to decide what to do with it.
instance (YTrans g g y t, YTrans g g y [t]) => YTrans Cx.ListF g y [t] where
  ytrans (Cx.NilF :&: l) = return $ IDs []
  ytrans ((Cx.ConsF x xs) :&: l) = do
    id :: ID y t <- ytranslate x
    (IDs ids) :: ID y [t] <- ytranslate xs
    return $ IDs ((id, Occurrence [l]) : ids)

instance (Cx.Ident :<: g, IdentF :<: y) => YTrans Cx.Ident g y AddressT where
  ytrans (Cx.Ident name :&: l) = insertNode [l] (IdentF name)

instance (Cx.Assign :<: g, AssignF :<: y, MemF :<: y, YTrans g g y AddressT, YTrans g g y ValueT) => YTrans Cx.Assign g y MemValT where
  ytrans (Cx.Assign lv _ rv :&: l) = AssignF <$> ytranslate rv <*> ytranslate lv <*> getMem >>= insertNode [l] >>= updateMemVal
