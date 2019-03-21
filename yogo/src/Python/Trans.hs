{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Python.Trans (
  YPythonSig
  , PyLhs(..)
  , PyListLV(..)
  , PyOp(..)
  , PyOp'(..)
  , PyArgKeyword(..)
  , PyArgSplat(..)
  , PyArgKWSplat(..)
  , PyTuple(..)
  , PyList(..)
  , PyDict(..)
  , PySet(..)
  , toGraphPython
  ) where

import Data.Proxy ( Proxy(..) )
import Debug.Trace
import Data.Typeable

import Control.Lens ( makeLenses, over, use, (^.), (.=), (%=), (+=), (-=) )
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
import qualified Cubix.Language.Parametric.Syntax as Cx
import qualified Cubix.Language.Python.Parametric.Common as Py

import Common.Trans

-- Represent a compound address for multiple assignment, as in a = b = 0
data PyLhs e l where
  PyLhs :: e AddressT -> e AddressT -> PyLhs e AddressT

-- Represent destructurable address, as in ((a, b), c) = ((1, 2), 3)
data PyListLV e l where
  PyListLV :: e AddressT -> e AddressT -> PyListLV e AddressT

data PyArgKeyword e l where
  PyArgKeyword :: e AddressT -> e ValueT -> PyArgKeyword e ValueT

data PyArgSplat e l where
  PyArgSplat :: e ValueT -> PyArgSplat e ValueT

data PyArgKWSplat e l where
  PyArgKWSplat :: e ValueT -> PyArgKWSplat e ValueT

data PyTuple e l where
  -- PyTuple head rest
  PyTuple :: e l -> e l -> PyTuple e l

data PyList e l where
  -- PyList head rest
  PyList :: e l -> e l -> PyList e l

data PyDict (e :: * -> *) l where
  -- key, value, dict
  PyDict :: e ValueT -> e ValueT -> e ValueT -> PyDict e ValueT

data PySet (e :: * -> *) l where
  PySet :: e ValueT -> e ValueT -> PySet e ValueT

data PyOp' = PyIn | PyNotIn
data PyOp (e :: * -> *) t where
  PyOp :: PyOp' -> PyOp e OpT

type YPythonSig = PySet :+: PyDict :+: PyList :+: PyTuple :+: PyArgKWSplat :+: PyArgSplat :+:PyArgKeyword :+: PyOp :+: PyListLV :+: PyLhs :+: YGenericSig
type PyID t = ID YPythonSig t

type CanYTrans f = (HFunctor f, ShowHF f, Typeable f)
type MonadYogoPy m = (MonadYogo YPythonSig m)

class (CanYTrans f) => YTrans f t where
  ytrans :: (MonadYogoPy m) => YTransM m f Py.MPythonSig YPythonSig t

ytranslate :: (MonadYogoPy m, YTrans Py.MPythonSig t) => YTransTermM m Py.MPythonSig YPythonSig t
ytranslate f = (ytrans . unTerm) f

-- ----------
-- Generic
-- ----------

instance {-# OVERLAPPABLE #-} (CanYTrans f) => YTrans f t where
  ytrans t = do
    traceM $ show $ typeRep (Proxy :: Proxy f)
    mzero
  -- ytrans = error $ show (typeRep (Proxy :: Proxy f))

instance {-# OVERLAPPING #-} (YTrans f1 t, YTrans f2 t) => YTrans (f1 :+: f2) t where
  ytrans = caseH' ytrans ytrans

instance (YTrans Py.MPythonSig t) => YTrans Cx.ListF [t] where
  ytrans = ytransList ytranslate ytranslate

instance YTrans Cx.Ident AddressT where
  ytrans (Cx.Ident name :&: l) = insertNode [l] (IdentF name)

instance YTrans Cx.Assign MemValT where
  ytrans = ytransAssign ytranslate ytranslate

instance YTrans Cx.FunctionCall MemValT where
  ytrans = ytransFCall ytranslate ytranslate ytranslate

instance YTrans Cx.PositionalArgument ValueT where
  ytrans (Cx.PositionalArgument t :&: _) = ytranslate t

-- Could be address or value, depending on whether the function has a receiver argument
instance YTrans Cx.FunctionIdent ValueT where
  ytrans (Cx.FunctionIdent t :&: _) = ytranslate t

instance YTrans Cx.FunctionIdent AddressT where
  ytrans (Cx.FunctionIdent t :&: _) = ytranslate t

instance YTrans Cx.FunctionDef StatementT where
  ytrans (Cx.FunctionDef _ fname _ fbody :&: l) = ytranslateScope ytranslate ytranslate [l] fname fbody

instance YTrans Cx.Block [StatementT] where
  ytrans (Cx.Block t _ :&: l) = ytranslate t

-- ----------
-- Python specific
-- ----------

instance YTrans Py.Op OpT where
  ytrans (Py.And _ :&: l) = insertNode [l] (CommonOp And)
  ytrans (Py.Or _ :&: l) = insertNode [l] (CommonOp Or)
  ytrans (Py.Not _ :&: l) = insertNode [l] (CommonOp Not)
  ytrans (Py.Exponent _ :&: l) = insertNode [l] (CommonOp Exponent)
  ytrans (Py.Plus _ :&: l) = insertNode [l] (CommonOp Plus)
  ytrans (Py.Minus _ :&: l) = insertNode [l] (CommonOp Minus)
  ytrans (Py.Multiply _ :&: l) = insertNode [l] (CommonOp Multiply)
  ytrans (Py.Divide _ :&: l) = insertNode [l] (CommonOp Divide)
  ytrans (Py.Modulo _ :&: l) = insertNode [l] (CommonOp Modulo)
  ytrans (Py.LessThan _ :&: l) = insertNode [l] (CommonOp LessThan)
  ytrans (Py.LessThanEquals _ :&: l) = insertNode [l] (CommonOp LessThanEquals)
  ytrans (Py.GreaterThan _ :&: l) = insertNode [l] (CommonOp GreaterThan)
  ytrans (Py.GreaterThanEquals _ :&: l) = insertNode [l] (CommonOp GreaterThanEquals)
  ytrans (Py.Equality _ :&: l) = insertNode [l] (CommonOp Equals)
  ytrans (Py.NotEquals _ :&: l) = insertNode [l] (CommonOp NotEquals)
  ytrans (Py.In _ :&: l) = insertNode [l] (PyOp PyIn)
  ytrans (Py.NotIn _ :&: l) = insertNode [l] (PyOp PyNotIn)
  ytrans (_ :&: l) = insertNode [l] (CommonOp Unknown)

instance YTrans Py.PAssignOp OpT where
  ytrans (Py.PlusAssign _ :&: l) = insertNode [l] (CommonOp Plus)
  ytrans (Py.MinusAssign _ :&: l) = insertNode [l] (CommonOp Minus)
  ytrans (Py.MultAssign _ :&: l) = insertNode [l] (CommonOp Multiply)
  ytrans (_ :&: l) = insertNode [l] (CommonOp Unknown)

ytransLhs :: (MonadYogoPy m, f :<: YPythonSig)
          => [Label]
          -> (PyID AddressT -> PyID AddressT -> f (ID YPythonSig) AddressT)
          -> Py.MPythonTermLab [t]
          -> m (PyID AddressT)
ytransLhs labels constructor t = do
  lvalues <- ytranslate t
  go (fromIds lvalues)
    where
      -- As an optimization, PyLhs (ConsF lv NilF) => lv.
      --- This is so that most assignments will bypass PyLhs altogether.
      go [] = error "Not expecting empty PyLhs"
      go [(l, id)] = return id
      go ((l, id) : xs) = go xs >>= (insertNode (l : labels)) . (constructor id)

instance YTrans Py.Expr AddressT where
  ytrans (Py.Var ident _ :&: l) = ytranslate ident
  ytrans (Py.Paren expr _ :&: _) = ytranslate expr
  -- TODO, make (x,) a list too
  ytrans (Py.Tuple t _ :&: l) = ytransLhs [l] PyListLV t
  ytrans (Py.List t _ :&: l) = ytransLhs [l] PyListLV t
  ytrans (Py.Dot m k _ :&: l) =
    DotF <$> ytranslate m <*> ytranslate k >>= insertNode [l]
  ytrans f = error "Py.expr AddressT Not Implemented"

ytransPyList :: (MonadYogoPy m, f :<: YPythonSig)
             => [Label]
             -> (ID YPythonSig ValueT -> ID YPythonSig ValueT -> f (ID YPythonSig) ValueT)
             -> Py.MPythonTermLab [t]
             -> m (PyID ValueT)
ytransPyList labels constructor t = do
  lvalues <- ytranslate t
  go (fromIds lvalues)
    where
      go [] = do
        nothing <- insertNode labels NothingF
        insertNode [] $ constructor nothing nothing
      go ((l, id) : xs) = go xs >>= (insertNode (l : labels)) . (constructor id)

ytransDict :: (MonadYogoPy m)
           => [Label]
           -> Py.MPythonTermLab [t]
           -> m (PyID ValueT)
ytransDict labels t@(project' -> Just (Cx.NilF)) = do
  nothing <- insertNode labels NothingF
  insertNode labels $ PyDict nothing nothing nothing
ytransDict labels t@(project' -> Just (Cx.ConsF pair rest)) =
  case project' pair of
    Just (Py.DictMappingPair key value) ->
      PyDict <$> ytranslate key <*> ytranslate value <*> ytransDict labels rest
      >>= insertNode ((getLabel t) : labels)
    _ -> error "ytransDict: not expecting anything other than DictMappingPair"

instance YTrans Py.Expr ValueT where
  ytrans (Py.Int n _ _ :&: l) = insertNode [l] (ConstF (IntegerF n))
  ytrans (Py.LongInt n _ _ :&: l) = insertNode [l] (ConstF (IntegerF n))
  ytrans (Py.Bool p _ :&: l) = insertNode [l] (ConstF (BoolF p))
  ytrans (Py.None _ :&: l) = insertNode [l] (ConstF NullF)
  ytrans (Py.Var ident _ :&: l) = ytranslate ident >>= iQ [l]
  ytrans (Py.Subscript m k _ :&: l) =
    SelF <$> ytranslate m <*> ytranslate k >>= insertNode [l] >>= iQ [l]
  ytrans (Py.BinaryOp op arg1 arg2 _ :&: l) =
    BinopF <$> ytranslate op <*> ytranslate arg1 <*> ytranslate arg2 >>= insertNode [l]
  ytrans (Py.UnaryOp op arg _ :&: l) =
    UnopF <$> ytranslate op <*> ytranslate arg >>= insertNode [l]
  ytrans (Py.Dot m k _ :&: l) =
    DotF <$> ytranslate m <*> ytranslate k >>= insertNode [l] >>= iQ [l]
  ytrans (Py.Tuple t _ :&: l) = ytransPyList [l] PyTuple t
  ytrans (Py.List t _ :&: l) = ytransPyList [l] PyList t
  ytrans (Py.Set t _ :&: l) = ytransPyList [l] PySet t
  ytrans (Py.Dictionary t _ :&: l) = ytransDict [l] t
  ytrans (Py.Paren expr _ :&: _) = ytranslate expr
  ytrans (_ :&: l) = traceM "Unknown Py.Expr ValueT" >> iUnknownF [l] >>= insertNode [l] . ValF

ytransChainComp :: (MonadYogoPy m)
                => Label -> PyID OpT -> PyID ValueT -> TermLab Py.MPythonSig Py.PyCompL
                -> m (PyID ValueT)
ytransChainComp l op1 arg1 t@(project' -> Just (Py.PyBaseComp op2 arg2 arg3)) = do
  op2 <- ytranslate op2
  arg2 <- ytranslate arg2
  arg3 <- ytranslate arg3
  binop1 <- insertNode [] $ BinopF op1 arg1 arg2
  binop2 <- insertNode [getLabel t] $ BinopF op2 arg2 arg3
  and <- insertNode [] $ CommonOp And
  insertNode [l] $ BinopF and binop1 binop2
ytransChainComp l op1 arg1 t@(project' -> Just (Py.PyChainComp op2 arg2 comp)) = do
  op2 <- ytranslate op2
  arg2 <- ytranslate arg2
  mem <- getMem
  binop1 <- insertNode [] $ BinopF op1 arg1 arg2
  binop2 <- ytransChainComp (getLabel t) op2 arg2 comp
  and <- insertNode [] $ CommonOp And
  insertNode [l] $ BinopF and binop1 binop2

instance YTrans Py.PyComp ValueT where
  ytrans (Py.PyBaseComp op arg1 arg2 :&: l) =
    BinopF <$> ytranslate op <*> ytranslate arg1 <*> ytranslate arg2 >>= insertNode [l]
  ytrans (Py.PyChainComp op arg comp :&: l) = do
    op <- ytranslate op
    arg <- ytranslate arg
    ytransChainComp l op arg comp

ytransConditional :: (MonadYogoPy m)
                  => Label
                  -> TermLab Py.MPythonSig [(Py.ExprL, [Py.StatementL])]
                  -> TermLab Py.MPythonSig [Py.StatementL]
                  -> m (PyID MemoryT)
ytransConditional condLabel (project' -> Just (Cx.NilF)) condElse = do
  _ :: PyID [StatementT] <- ytranslate condElse
  getMem
ytransConditional condLabel t@(project' -> Just (Cx.ConsF guard xs)) condElse =
  case project' guard of
    Just (Cx.PairF cond body) -> do
      cond <- ytranslate cond
      memBefore <- getMem >>= pushMem
      _ :: PyID [StatementT] <- ytranslate body
      memTrue <- popMem
      memFalse <- ytransConditional condLabel xs condElse
      memCond <- insertNode [condLabel, getLabel t] (CondMemF cond memTrue memFalse)
      updateMem memCond
    Nothing -> error "Unexpected error in ytransConditional"

instance YTrans Py.Statement StatementT where
  ytrans (Py.StmtExpr expr _ :&: _) =
    ytranslate expr >>= \(_ :: PyID ValueT) -> return Statement
  ytrans (Py.Conditional condGuards condElse _ :&: l) =
    ytransConditional l condGuards condElse >> return Statement
  ytrans (Py.While whileCond whileBody whileElse _ :&: l) = do
    temp <- enterLoop
    cond <- ytranslate whileCond
    _ :: PyID [StatementT] <- ytranslate whileBody
    exitLoop [l] temp cond
    case project' whileElse of
      Just f@(Cx.ConsF _ _) -> iUnknownF [getLabel whileElse] >> return Statement
      _ -> return Statement
  ytrans (Py.For targets generator body forElse _ :&: l) = do
    generator' <- ytranslate generator
    temp <- enterLoop
    iter <- iIterV [(getLabel generator)] generator'
    cond <- iIterP [(getLabel generator)] generator'
    targets <- ytransLhs [] PyListLV targets
    AssignF iter targets <$> getMem >>= insertNode [l] >>= updateMemVal
    _ :: PyID [StatementT] <- ytranslate body
    exitLoop [l] temp cond
    case project' forElse of
      Just f@(Cx.ConsF _ _) -> iUnknownF [getLabel forElse] >> return Statement
      _ -> return Statement
  ytrans (Py.Pass _ :&: _) = return Statement
  ytrans (Py.AugmentedAssign to op expr _ :&: l) = do
    lvalue <- ytranslate to
    binop <- BinopF <$> ytranslate op <*> iQ [getLabel to] lvalue <*> ytranslate expr
             >>= insertNode [l]
    AssignF binop lvalue <$> getMem >>= insertNode [l] >>= updateMemVal
    return Statement
  ytrans t@(Py.Try body _ _ _ _ :&: l) = do
    _ :: PyID [StatementT] <- ytranslate body
    iUnknownF [l] -- for else/excepts/etc.
    return Statement
  ytrans (_ :&: l) = traceM "Unknown statement" >> iUnknownF [l] >> return Statement

instance YTrans Py.Module StatementT where
  ytrans (Py.Module body :&: _) = do
    newScope $ Name "Module"
    _ :: PyID [StatementT] <- ytranslate body
    return Statement

instance YTrans Py.PyClass StatementT where
  ytrans (Py.PyClass cname _ cbody :&: l) = ytranslateScope ytranslate ytranslate [l] cname cbody

instance YTrans Py.DotLValue AddressT where
  ytrans (Py.DotLValue m k :&: l) =
    DotF <$> ytranslate m <*> ytranslate k >>= insertNode [l]

instance YTrans Py.SubscriptLValue AddressT where
  ytrans (Py.SubscriptLValue m k :&: l) =
    SelF <$> ytranslate m <*> ytranslate k >>= insertNode [l]

instance YTrans Py.PythonArg ValueT where
  ytrans (Py.PythonArgSplat val :&: l) = PyArgSplat <$> ytranslate val >>= insertNode [l]
  ytrans (Py.PythonArgKeyword param val :&: l) = PyArgKeyword <$> ytranslate param <*> ytranslate val >>= insertNode [l]
  ytrans (Py.PythonArgKWSplat val :&: l) = PyArgKWSplat <$> ytranslate val >>= insertNode [l]

instance YTrans Py.PyLhs AddressT where
  ytrans (Py.PyLhs t :&: l) = ytransLhs [l] PyLhs t

instance YTrans Py.TupleLValue AddressT where
  ytrans (Py.TupleLValue t :&: l) = ytransLhs [l] PyTuple t

instance YTrans Py.PyBlock [StatementT] where
  ytrans (Py.PyBlock _ t :&: _) = ytranslate t

instance YTrans Py.IdentIsIdent AddressT where
  ytrans (Py.IdentIsIdent t :&: _) = ytranslate t

instance YTrans Py.IdentIsPyLValue AddressT where
  ytrans (Py.IdentIsPyLValue t :&: _) = ytranslate t

instance YTrans Py.ExprIsRhs ValueT where
  ytrans (Py.ExprIsRhs t :&: _) = ytranslate t

instance YTrans Py.ExprIsPositionalArgExp ValueT where
  ytrans (Py.ExprIsPositionalArgExp t :&: _) = ytranslate t

instance YTrans Py.ExprIsReceiver ValueT where
  ytrans (Py.ExprIsReceiver t :&: _) = ytranslate t

instance YTrans Py.ExprIsFunctionExp ValueT where
  ytrans (Py.ExprIsFunctionExp t :&: _) = ytranslate t

instance YTrans Py.ExprIsFunctionExp AddressT where
  ytrans (Py.ExprIsFunctionExp t :&: _) = ytranslate t

instance YTrans Py.FunctionCallIsExpr ValueT where
  ytrans (Py.FunctionCallIsExpr t :&: l) = ytranslate t >>= (insertNode [l]) . ValF

instance YTrans Py.PyCompIsExpr ValueT where
  ytrans (Py.PyCompIsExpr t :&: _) = ytranslate t

instance YTrans Py.AssignIsStatement StatementT where
  ytrans (Py.AssignIsStatement t :&: _) = ytranslate t >>= \(_ :: PyID MemValT) -> return Statement

instance YTrans Py.FunctionDefIsStatement StatementT where
  ytrans (Py.FunctionDefIsStatement t :&: _) = ytranslate t

instance YTrans Py.PyClassIsStatement StatementT where
  ytrans (Py.PyClassIsStatement t :&: _) = ytranslate t

instance YTrans Py.StatementIsBlockItem StatementT where
  ytrans (Py.StatementIsBlockItem t :&: _) = ytranslate t

instance YTrans Py.PyBlockIsFunctionBody [StatementT] where
  ytrans (Py.PyBlockIsFunctionBody t :&: _) = ytranslate t

ytransPythonModule :: (MonadYogoPy m) => TranslateM m Py.MPythonTermLab Py.ModuleL (PyID StatementT)
ytransPythonModule t = ytranslate t

initState :: YogoState YPythonSig
initState = YogoState [] [] Map.empty 0 0

fileToGraphPython :: Py.MPythonTermLab l -> YFile YPythonSig
fileToGraphPython t =
  let (a, state) = fromJust $ runIdentity $ runMaybeT $ runStateT ((onetdT $ promoteTF ytransPythonModule) t) initState in
    state ^. file

toGraphPython :: Project Py.MPythonSig -> YProject YPythonSig
toGraphPython = Map.map (\(E t) -> fileToGraphPython t)
