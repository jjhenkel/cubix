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
{-# LANGUAGE ViewPatterns #-}

module Common.Trans where

import Data.Proxy ( Proxy(..) )
import Debug.Trace
import Data.Typeable

import Control.Lens ( makeLenses, over, use, (.=), (%=), (+=), (-=) )
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
data FArgT

-- Syntax types. Do not correspond to any node
data OpT -- Operators
data StatementT

type Depth = Int

data Primitive = IntegerF Integer | IntF Int | BoolF Bool | StringF String | NullF

data CommonOp' = And | Or | Not
               | Exponent | Plus | Minus | Multiply | Divide | Modulo
               | LessThan | GreaterThan | Equals | GreaterThanEquals | LessThanEquals | NotEquals
               | Unknown

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

data MemGenesisF (e :: * -> *) t where
  MemGenesisF :: MemGenesisF e MemoryT

data MemF e t where
  MemF :: e MemValT -> MemF e MemoryT

data ValF e t where
  ValF :: e MemValT -> ValF e ValueT

data UnknownF e t where
  UnknownF :: e MemoryT -> UnknownF e MemValT

data NothingF (e :: * -> *) t where
  NothingF :: NothingF e t

data TempF (e :: * -> *) t where
  TempF :: Int -> Depth -> TempF e t

data ConstF (e :: * -> *) t where
  ConstF :: Primitive -> ConstF e ValueT

data IdentF (e :: * -> *) t where
  IdentF :: String -> IdentF e AddressT

data SelF e t where
  SelF :: e ValueT -> e ValueT -> SelF e AddressT

data AtF e t where
  AtF :: e ValueT -> AtF e AddressT

data DotF e t where
  -- object.ident
  DotF :: e ValueT -> e AddressT -> DotF e AddressT

-- (deref mem ref) = (q mem (at mem ref))
-- This is a lazy version so that we don't over-generate at-nodes, which is an lvalue
data DerefF e t where
  DerefF :: e ValueT -> e MemoryT -> DerefF e ValueT

-- Pure operations, don't deref their arguments and don't depend on memory
data UnopF e t where
  UnopF :: e OpT -> e ValueT -> UnopF e ValueT

data BinopF e t where
  BinopF :: e OpT -> e ValueT -> e ValueT -> BinopF e ValueT

-- Arguments by order of execution
data AssignF e t where
  AssignF :: e ValueT -> e AddressT -> e MemoryT -> AssignF e MemValT

-- Function call implicitly deref their arguments
data FunctionCallF e t where
  FunctionCallF :: e ValueT -> e [FArgT] -> e MemoryT -> FunctionCallF e MemValT

data FunctionArgsF e t where
  FunctionArgsF :: e ValueT -> e [FArgT] -> FunctionArgsF e [FArgT]

data CondF e t where
  CondF :: e ValueT -> e t -> e t -> CondF e t

data CondMemF e t where
  CondMemF :: e ValueT -> e MemoryT -> e MemoryT -> CondMemF e MemoryT

data LoopF e t where
  -- depth, init, next
  LoopF :: Depth -> e ValueT -> e ValueT -> LoopF e ValueT

data LoopMemF e t where
  -- depth, init, next
  LoopMemF :: Depth -> e MemoryT -> e MemoryT -> LoopMemF e MemoryT

data FinalF e t where
  -- depth, cond, loop
  FinalF :: Depth -> e ValueT -> e ValueT -> FinalF e ValueT

data FinalMemF e t where
  -- depth, cond, loop
  FinalMemF :: Depth -> e ValueT -> e MemoryT -> FinalMemF e MemoryT

data IterVF e t where
  IterVF :: Depth -> e ValueT -> IterVF e ValueT

data IterPF e t where
  IterPF :: Depth -> e ValueT -> IterPF e ValueT

type YGenericSig = IterPF :+: IterVF :+: FinalMemF :+: FinalF :+: LoopMemF :+: LoopF :+: CondMemF :+: CondF :+: FunctionArgsF :+: FunctionCallF :+: AssignF :+: UnopF :+: BinopF :+: DerefF :+: DotF :+: AtF :+: SelF :+: IdentF :+: ConstF :+: TempF :+: NothingF :+: UnknownF :+: ValF :+: MemF :+: MemGenesisF :+: AnyHeap :+: AnyStack :+: CommonOp :+: Q :+: AnyMem :+: AnyLValue :+: AnyNode

newtype Name = Name [Char] deriving (Eq, Show, Ord)
newtype Occurrence = Occurrence [Label] deriving (Eq, Show)

data ID (f :: (* -> *) -> * -> *) t where
  ID :: Int -> ID f t
  IDs :: [(Label, ID f t)] -> ID f [t]
  Statement :: ID f StatementT

instance Show (ID f t) where
  show (ID n) = "(ID " ++ show n ++ ")"
  show (IDs ids) = "[IDS " ++ (intercalate " " $ map show ids) ++ "]"
  show Statement = "Statement"

fromIds :: ID f [t] -> [(Label, ID f t)]
fromIds (IDs ids) = ids
fromIds t = error $ "ID is not a list: " ++ show t

data Node (f :: (* -> *) -> * -> *) t = Node (f (ID f) t)

data YGraphEntry f = YGraphNode (E (ID f)) (E (Node f)) Occurrence
                   | YGraphEq (E (ID f)) (E (ID f))

type YGraph f = [YGraphEntry f]
type YFile f = Map Name (YGraph f)
type YProject f = Map FilePath (YFile f)

data YogoState f = YogoState { _nameScope :: [Name]
                             , _memScope :: [ID f MemoryT]
                             , _file  :: YFile f
                             , _counter :: Int
                             , _loopDepth :: Depth
                             }
makeLenses ''YogoState

type MonadYogo f m = (MonadPlus m, MonadState (YogoState f) m)

type YTransSortM m f g l y t = TranslateM m ((f :&: Label) (TermLab g)) l (ID y t)
type YTransM     m f g y t   = GTranslateM m ((f :&: Label) (TermLab g)) (ID y t)
type YTransTermM m f y t     = GTranslateM m (TermLab f) (ID y t)

instance Show (YGraphEntry f) where
  show (YGraphNode (E id) _ _) = show id
  show (YGraphEq (E id1) (E id2)) = "Eq " ++ show id1 ++  " " ++ show id2

getScopeName :: (MonadYogo f m) => m Name
getScopeName = liftM head $ use nameScope

newScope :: (MonadYogo f m, MemGenesisF :<: f) => Name -> m (ID f MemoryT)
newScope name = do
  nameScope %= (name :)
  file %= Map.insert name []
  insertNode [] MemGenesisF >>= pushMem

endScope :: (MonadYogo f m) => m ()
endScope = do
  nameScope %= tail
  popMem
  return ()

getCounter :: (MonadYogo f m) => m Int
getCounter = counter += 1 >> use counter

getNextID :: (MonadYogo f m) => m (ID f t)
getNextID = getCounter >>= return . ID

enterLoop :: (MonadYogo f m, TempF :<: f) => m (ID f MemoryT)
enterLoop = do
  loopDepth += 1
  TempF <$> getCounter <*> use loopDepth >>= insertNode [] >>= pushMem

exitLoop :: (MonadYogo f m, LoopMemF :<: f, FinalMemF :<: f)
         => [Label] -> ID f MemoryT -> ID f ValueT -> m (ID f MemoryT)
exitLoop labels temp cond = do
  next <- popMem
  init <- popMem
  depth <- use loopDepth
  loop <- insertNode labels $ LoopMemF depth init next
  addEntry $ YGraphEq (E temp) (E loop)
  final <- insertNode [] $ FinalMemF depth cond loop
  loopDepth -= 1
  pushMem final

addEntry :: (MonadYogo f m) => YGraphEntry f -> m ()
addEntry entry = getScopeName >>= (file %=) . (Map.adjust (entry :))

insertNode' :: (MonadYogo f m, g :<: f) => Occurrence -> g (ID f) t -> m (ID f t)
insertNode' occ node = do
  id <- getNextID
  addEntry $ YGraphNode (E id) ((E . Node . inj) node) occ
  return id

insertNode :: (MonadYogo f m, g :<: f) => [Label] -> g (ID f) t -> m (ID f t)
insertNode labels = insertNode' (Occurrence labels)

iMemF :: (MonadYogo f m, MemF :<: f) => [Label] -> ID f MemValT -> m (ID f MemoryT)
iMemF labels memVal = insertNode labels (MemF memVal) >>= updateMem

iQ :: (MonadYogo f m, Q :<: f) => [Label] -> ID f AddressT -> m (ID f ValueT)
iQ labels lvalue = getMem >>= (insertNode labels) . (Q lvalue)

iIterV :: (MonadYogo f m, IterVF :<: f) => [Label] -> ID f ValueT -> m (ID f ValueT)
iIterV labels iterable = use loopDepth >>= \depth -> insertNode labels (IterVF depth iterable)

iIterP :: (MonadYogo f m, IterPF :<: f) => [Label] -> ID f ValueT -> m (ID f ValueT)
iIterP labels iterable = use loopDepth >>= \depth -> insertNode labels (IterPF depth iterable)

iUnknownF :: (MonadYogo f m, UnknownF :<: f, MemF :<: f) => [Label] -> m (ID f MemValT)
iUnknownF labels = getMem >>= (insertNode labels) . UnknownF >>= updateMemVal

iAssignF :: (MonadYogo f m, AssignF :<: f, MemF :<: f) => [Label] -> ID f ValueT -> ID f AddressT -> m (ID f MemValT)
iAssignF labels rv lv = getMem >>= (insertNode labels) . (AssignF rv lv) >>= updateMemVal

getMem :: (MonadYogo f m) => m (ID f MemoryT)
getMem = liftM head $ use memScope

pushMem :: (MonadYogo f m) => ID f MemoryT -> m (ID f MemoryT)
pushMem mem = memScope %= (mem :) >> return mem

popMem :: (MonadYogo f m) => m (ID f MemoryT)
popMem = getMem >>= \mem -> memScope %= tail >> return mem

updateMem :: (MonadYogo f m) => ID f MemoryT -> m (ID f MemoryT)
updateMem mem = popMem >> pushMem mem

updateMemVal :: (MonadYogo f m, MemF :<: f) => ID f MemValT -> m (ID f MemValT)
updateMemVal memVal = iMemF [] memVal >> return memVal

getLabel :: TermLab f t -> Label
getLabel (Term (_ :&: l)) = l

-- Not every kinds of ListF needs to become a list in Yogo graph, therefore just
-- return ID f [t] for the caller to decide what to do with it.
ytransList :: (MonadYogo y m, Cx.ListF :<: f)
           => YTransTermM m f y t
           -> YTransTermM m f y [t]
           -> YTransM m Cx.ListF f y [t]
ytransList _ _ (Cx.NilF :&: _) = return $ IDs []
ytransList ytElem ytList (Cx.ConsF x xs :&: l) = do
  id <- ytElem x
  (IDs ids) <- ytList xs
  return $ IDs $ (l, id) : ids

type AssignFragment f y = (Cx.Assign :<: f, Cx.AssignOpEquals :<: f, AssignF :<: y, MemF :<: y)

ytransAssign :: (MonadYogo y m, AssignFragment f y)
             => YTransTermM m f y AddressT
             -> YTransTermM m f y ValueT
             -> YTransM m Cx.Assign f y MemValT
ytransAssign ytAddr ytVal (Cx.Assign lv (project' -> Just (Cx.AssignOpEquals)) rv :&: l) = do
  rvalue <- ytVal rv
  lvalue <- ytAddr lv
  iAssignF [l] rvalue lvalue

type FunctionCallFragment f y = (Cx.FunctionCall :<: f, Cx.ListF :<: f, Cx.ReceiverArg :<: f,
                                 Cx.FunctionArgumentList :<: f, Cx.PositionalArgument :<: f,
                                 NothingF :<: y, FunctionCallF :<: y, FunctionArgsF :<: y,
                                 MemF :<: y, DotF :<: y, Q :<: y)

ytransFCall :: (MonadYogo y m, FunctionCallFragment f y)
            => YTransTermM m f y AddressT
            -> YTransTermM m f y ValueT
            -> YTransTermM m f y [ValueT]
            -> YTransM m Cx.FunctionCall f y MemValT
ytransFCall ytAddr ytVal ytList (Cx.FunctionCall _ fexpr fargs :&: l) = do
  -- Check if there is a receiver
  recvArgs <- case project' fargs of
                Just (Cx.FunctionArgumentList list) ->
                  case project' list of
                    Just (Cx.ConsF arg args) ->
                      case project' arg of
                        Just (Cx.ReceiverArg recv) -> ytVal recv >>= \recv -> return $ Just (recv, getLabel arg, args)
                        _ -> return Nothing
                    _ -> return Nothing
  fn <- case recvArgs of
          Nothing -> ytVal fexpr
          Just (recv, l, _) -> ytAddr fexpr >>= insertNode [getLabel fexpr] . (DotF recv) >>= iQ [getLabel fexpr, l]
  let argsTerm = case recvArgs of
                  Nothing -> case project' fargs of
                               Just (Cx.FunctionArgumentList list) -> list
                  Just (_, _, args) -> args
  args <- ytList argsTerm >>= case recvArgs of
                                Nothing -> parseArgs . fromIds
                                Just (recv, l, _) -> parseArgs . ((l, recv) :) . fromIds
  getMem >>= (insertNode [l]) . (FunctionCallF fn args) >>= updateMemVal
  where
    parseArgs [] = insertNode [] NothingF
    parseArgs ((l, id) : xs) = parseArgs xs >>= (insertNode [l]) . (FunctionArgsF id)


type ScopeFragment f y = (Cx.Ident :<: f,
                          MemGenesisF :<: y, UnknownF :<: y,
                          MemF :<: y, ValF :<: y, AssignF :<: y)

ytranslateScope :: (ScopeFragment f y, MonadYogo y m)
                => YTransTermM m f y [StatementT]
                -> YTransTermM m f y AddressT
                -> [Label]
                -> TermLab f lname
                -> TermLab f lbody
                -> m (ID y StatementT)
ytranslateScope ytStmts ytAddr labels name body = do
  let name' = case project' name of
                Just (Cx.Ident s) -> s
                _ -> error "ytransScope: unexpected name"
  scopeName <- liftM (Name . (name' ++) . show) getCounter
  newScope scopeName
  _ :: ID y [StatementT] <- ytStmts body
  endScope
  before <- getMem
  rvalue <- iUnknownF [getLabel body] >>= insertNode labels . ValF
  after <- getMem
  addEntry $ YGraphEq (E before) (E after)
  AssignF rvalue <$> ytAddr name <*> getMem >>= insertNode labels
  return Statement

type VarDeclFragment f y = (Cx.SingleLocalVarDecl :<: f, Cx.OptLocalVarInit :<: f, AssignF :<: y, MemF :<: y)

ytransSingleLocalVarDecl :: (MonadYogo y m, VarDeclFragment f y)
                         => YTransTermM m f y AddressT
                         -> YTransTermM m f y ValueT
                         -> YTransM m Cx.SingleLocalVarDecl f y StatementT
ytransSingleLocalVarDecl ytAddr _ (Cx.SingleLocalVarDecl _ binder (project' -> Just Cx.NoLocalVarInit) :&: _) = do
    ytAddr binder
    return Statement
ytransSingleLocalVarDecl ytAddr ytVal (Cx.SingleLocalVarDecl _ binder (project' -> Just (Cx.JustLocalVarInit init)) :&: l) = do
    var <- ytAddr binder
    val <- ytVal init
    iAssignF [l] val var
    return Statement
