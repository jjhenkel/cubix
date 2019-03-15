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

data PyOp' = PyIn | PyNotIn
data PyOp (e :: * -> *) t where
  PyOp :: PyOp' -> PyOp e OpT

type YPythonSig = PyArgKWSplat :+: PyArgSplat :+:PyArgKeyword :+: PyOp :+: PyListLV :+: PyLhs :+: YGenericSig
type PyID t = ID YPythonSig t
type YTranslatePyM m f t = GTranslateM m ((f :&: Label) Py.MPythonTermLab) (ID YPythonSig t)

type MonadYogoPy m = (MonadYogo YPythonSig m)
type CanYTransPy f = (CanYTrans f)

instance YTrans Py.Op Py.MPythonSig YPythonSig OpT where
  ytrans (Py.And _ :&: l) = insertNode [l] (CommonOp And)
  ytrans (Py.Or _ :&: l) = insertNode [l] (CommonOp Or)
  ytrans (Py.Not _ :&: l) = insertNode [l] (CommonOp Not)
  ytrans (Py.Exponent _ :&: l) = insertNode [l] (CommonOp Exponent)
  ytrans (Py.Plus _ :&: l) = insertNode [l] (CommonOp Plus)
  ytrans (Py.Minus _ :&: l) = insertNode [l] (CommonOp Minus)
  ytrans (Py.Multiply _ :&: l) = insertNode [l] (CommonOp Multiply)
  ytrans (Py.LessThan _ :&: l) = insertNode [l] (CommonOp LessThan)
  ytrans (Py.LessThanEquals _ :&: l) = insertNode [l] (CommonOp LessThanEquals)
  ytrans (Py.GreaterThan _ :&: l) = insertNode [l] (CommonOp GreaterThan)
  ytrans (Py.GreaterThanEquals _ :&: l) = insertNode [l] (CommonOp GreaterThanEquals)
  ytrans (Py.Equality _ :&: l) = insertNode [l] (CommonOp Equals)
  ytrans (Py.NotEquals _ :&: l) = insertNode [l] (CommonOp NotEquals)
  ytrans (Py.In _ :&: l) = insertNode [l] (PyOp PyIn)
  ytrans (Py.NotIn _ :&: l) = insertNode [l] (PyOp PyNotIn)
  ytrans _ = error "Py.Op Not Implemented"

ytransLhs :: (MonadYogoPy m, YTrans Py.MPythonSig Py.MPythonSig YPythonSig [t])
          => [Label] -> Py.MPythonTermLab [t] -> m (PyID AddressT)
ytransLhs labels t = do
  lvalues <- ytranslate t
  go (fromIds lvalues)
    where
      -- As an optimization, PyLhs (ConsF lv NilF) => lv.
      --- This is so that most assignments will bypass PyLhs altogether.
      go [] = error "Not expecting empty PyLhs"
      go [(l, id)] = return id
      go ((l, id) : xs) = go xs >>= (insertNode (l : labels)) . (PyLhs id)

ytransListLV :: (MonadYogoPy m, YTrans Py.MPythonSig Py.MPythonSig YPythonSig [t])
             => [Label] -> Py.MPythonTermLab [t] -> m (PyID AddressT)
ytransListLV labels t = do
  lvalues <- ytranslate t
  go (fromIds lvalues)
    where
      go [] = error "Not expecting empty PyLhs"
      go [(l, id)] = return id
      go ((l, id) : xs) = go xs >>= (insertNode (l : labels)) . (PyListLV id)

instance YTrans Py.Expr Py.MPythonSig YPythonSig AddressT where
  ytrans (Py.Var ident _ :&: l) = ytranslate ident
  ytrans (Py.Paren expr _ :&: _) = ytranslate expr
  -- TODO, make (x,) a list too
  ytrans (Py.Tuple t _ :&: l) = ytransListLV [l] t
  ytrans (Py.Dot m k _ :&: l) =
    DotF <$> ytranslate m <*> ytranslate k >>= insertNode [l]
  ytrans f = error "Py.expr AddressT Not Implemented"

instance YTrans Py.Expr Py.MPythonSig YPythonSig ValueT where
  ytrans (Py.Int n _ _ :&: l) = insertNode [l] (ConstF (IntegerF n))
  ytrans (Py.Var ident _ :&: l) = ytranslate ident >>= iQ [l]
  ytrans (Py.Subscript m k _ :&: l) =
    SelF <$> ytranslate m <*> ytranslate k >>= insertNode [l] >>= iQ [l]
  ytrans (Py.BinaryOp op arg1 arg2 _ :&: l) =
    BinopF <$> ytranslate op <*> ytranslate arg1 <*> ytranslate arg2 >>= insertNode [l]
  ytrans (Py.UnaryOp op arg _ :&: l) =
    UnopF <$> ytranslate op <*> ytranslate arg >>= insertNode [l]
  ytrans (Py.Dot m k _ :&: l) =
    DotF <$> ytranslate m <*> ytranslate k >>= insertNode [l] >>= iQ [l]
  ytrans (Py.Paren expr _ :&: _) = ytranslate expr
  ytrans f = error "Py.expr Value Not Implemented"

ytransChainComp :: (YTrans Py.MPythonSig Py.MPythonSig YPythonSig ValueT,
                    YTrans Py.MPythonSig Py.MPythonSig YPythonSig OpT,
                    MonadYogoPy m)
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

instance YTrans Py.PyComp Py.MPythonSig YPythonSig ValueT where
  ytrans (Py.PyBaseComp op arg1 arg2 :&: l) =
    BinopF <$> ytranslate op <*> ytranslate arg1 <*> ytranslate arg2 >>= insertNode [l]
  ytrans (Py.PyChainComp op arg comp :&: l) = do
    op <- ytranslate op
    arg <- ytranslate arg
    ytransChainComp l op arg comp

ytransConditional :: (YTrans Py.MPythonSig Py.MPythonSig YPythonSig [StatementT], MonadYogoPy m)
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

instance YTrans Py.Statement Py.MPythonSig YPythonSig StatementT where
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
      Just f@(Cx.ConsF _ _) -> ytransUnknown (f :&: getLabel whileElse) >> return Statement
      _ -> return Statement

  ytrans (Py.For targets generator body forElse _ :&: l) = do
    generator' <- ytranslate generator
    temp <- enterLoop
    iter <- iIterV [(getLabel generator)] generator'
    cond <- iIterP [(getLabel generator)] generator'
    targets <- ytransListLV [] targets
    AssignF iter targets <$> getMem >>= insertNode [l] >>= updateMemVal
    _ :: PyID [StatementT] <- ytranslate body
    exitLoop [l] temp cond
    case project' forElse of
      Just f@(Cx.ConsF _ _) -> ytransUnknown (f :&: getLabel forElse) >> return Statement
      _ -> return Statement

  ytrans f = error "Py.Statement Not Implemented"

instance YTrans Py.Module Py.MPythonSig YPythonSig ScopeT where
  ytrans (Py.Module body :&: _) = do
    newScope $ Name "Module"
    _ :: PyID [StatementT] <- ytranslate body
    return Scope

instance YTrans Py.DotLValue Py.MPythonSig YPythonSig AddressT where
  ytrans (Py.DotLValue m k :&: l) =
    DotF <$> ytranslate m <*> ytranslate k >>= insertNode [l]

instance YTrans Py.SubscriptLValue Py.MPythonSig YPythonSig AddressT where
  ytrans (Py.SubscriptLValue m k :&: l) =
    SelF <$> ytranslate m <*> ytranslate k >>= insertNode [l]

instance YTrans Py.PythonArg Py.MPythonSig YPythonSig ValueT where
  ytrans (Py.PythonArgSplat val :&: l) = PyArgSplat <$> ytranslate val >>= insertNode [l]
  ytrans (Py.PythonArgKeyword param val :&: l) = PyArgKeyword <$> ytranslate param <*> ytranslate val >>= insertNode [l]
  ytrans (Py.PythonArgKWSplat val :&: l) = PyArgKWSplat <$> ytranslate val >>= insertNode [l]

instance YTrans Py.PyLhs Py.MPythonSig YPythonSig AddressT where
  ytrans (Py.PyLhs t :&: l) = ytransLhs [l] t

instance YTrans Py.PyBlock Py.MPythonSig YPythonSig [StatementT] where
  ytrans (Py.PyBlock _ t :&: _) = ytranslate t

instance YTrans Py.IdentIsIdent Py.MPythonSig YPythonSig AddressT where
  ytrans (Py.IdentIsIdent t :&: _) = ytranslate t

instance YTrans Py.IdentIsPyLValue Py.MPythonSig YPythonSig AddressT where
  ytrans (Py.IdentIsPyLValue t :&: _) = ytranslate t

instance YTrans Py.ExprIsRhs Py.MPythonSig YPythonSig ValueT where
  ytrans (Py.ExprIsRhs t :&: _) = ytranslate t

instance YTrans Py.ExprIsPositionalArgExp Py.MPythonSig YPythonSig ValueT where
  ytrans (Py.ExprIsPositionalArgExp t :&: _) = ytranslate t

instance YTrans Py.ExprIsReceiver Py.MPythonSig YPythonSig ValueT where
  ytrans (Py.ExprIsReceiver t :&: _) = ytranslate t

instance YTrans Py.ExprIsFunctionExp Py.MPythonSig YPythonSig ValueT where
  ytrans (Py.ExprIsFunctionExp t :&: _) = ytranslate t

instance YTrans Py.ExprIsFunctionExp Py.MPythonSig YPythonSig AddressT where
  ytrans (Py.ExprIsFunctionExp t :&: _) = ytranslate t

instance YTrans Py.FunctionCallIsExpr Py.MPythonSig YPythonSig ValueT where
  ytrans (Py.FunctionCallIsExpr t :&: l) = ytranslate t >>= (insertNode [l]) . ValF

instance YTrans Py.PyCompIsExpr Py.MPythonSig YPythonSig ValueT where
  ytrans (Py.PyCompIsExpr t :&: _) = ytranslate t

instance YTrans Py.AssignIsStatement Py.MPythonSig YPythonSig StatementT where
  ytrans (Py.AssignIsStatement t :&: _) = ytranslate t >>= \(_ :: PyID MemValT) -> return Statement

instance YTrans Py.FunctionDefIsStatement Py.MPythonSig YPythonSig StatementT where
  ytrans (Py.FunctionDefIsStatement t :&: _) = ytranslate t

instance YTrans Py.StatementIsBlockItem Py.MPythonSig YPythonSig StatementT where
  ytrans (Py.StatementIsBlockItem t :&: _) = ytranslate t

instance YTrans Py.PyBlockIsFunctionBody Py.MPythonSig YPythonSig [StatementT] where
  ytrans (Py.PyBlockIsFunctionBody t :&: _) = ytranslate t

ytransPythonModule :: (MonadYogoPy m)
                   => TranslateM m Py.MPythonTermLab Py.ModuleL (ID YPythonSig ScopeT)
ytransPythonModule = ytranslate

initState :: YogoState YPythonSig
initState = YogoState [] [] Map.empty 0 0

fileToGraphPython :: Py.MPythonTermLab l -> YFile YPythonSig
fileToGraphPython t =
  let (a, state) = fromJust $ runIdentity $ runMaybeT $ runStateT ((onetdT $ promoteTF ytransPythonModule) t) initState in
    state ^. file

toGraphPython :: Project Py.MPythonSig -> YProject YPythonSig
toGraphPython = Map.map (\(E t) -> fileToGraphPython t)
