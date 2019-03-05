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

module Python.Trans (
  YPythonSig
  , PyLhs(..)
  , toGraphPython
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
import qualified Cubix.Language.Python.Parametric.Common as Py

import Common.Trans

-- Represent a compound address
data PyLhs e l where
  PyLhs :: e AddressT -> e AddressT -> PyLhs e AddressT

type YPythonSig = PyLhs :+: YGenericSig
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
  ytrans (Py.NotEquals _ :&: l) = insertNode [l] (CommonOp NotEquals)
  ytrans _ = error "Py.Op Not Implemented"

instance YTrans Py.Expr Py.MPythonSig YPythonSig ValueT where
  ytrans (Py.Int n _ _ :&: l) = insertNode [l] (ConstF (IntegerF n))
  ytrans (Py.Var ident _ :&: l) = ytranslate ident >>= iQ [l]
  ytrans (Py.BinaryOp op arg1 arg2 _ :&: l) =
    BinOpF <$> ytranslate op <*> ytranslate arg1 <*> ytranslate arg2 >>= insertNode [l]
  ytrans (Py.UnaryOp op arg _ :&: l) =
    UnOpF <$> ytranslate op <*> ytranslate arg >>= insertNode [l]
  ytrans (Py.Paren expr _ :&: _) = ytranslate expr
  ytrans f = error "Py.expr Not Implemented"

instance YTrans Py.Statement Py.MPythonSig YPythonSig StatementT where
  ytrans (Py.StmtExpr expr _ :&: _) = ytranslate expr >>= \(_ :: PyID ValueT) -> return Statement
  ytrans f = error "Py.Statement Not Implemented"

instance YTrans Py.Module Py.MPythonSig YPythonSig ScopeT where
  ytrans (Py.Module body :&: _) = do
    newScope $ Name "Module"
    _ :: PyID [StatementT] <- ytranslate body
    return Scope

instance YTrans Py.PyLhs Py.MPythonSig YPythonSig AddressT where
  ytrans (Py.PyLhs t :&: l) = do
    lvalues <- ytranslate t
    go (fromIds lvalues)
      where
        -- As an optimization, PyLhs (ConsF lv NilF) => lv. This is so that most assignments will bypass PyLhs altogether.
        go [] = error "Not expecting empty PyLhs"
        go [(id, l')] = return id
        go ((id, l') : xs) = go xs >>= (insertNode [l, l']) . (PyLhs id)

instance YTrans Py.IdentIsIdent Py.MPythonSig YPythonSig AddressT where
  ytrans (Py.IdentIsIdent t :&: _) = ytranslate t

instance YTrans Py.IdentIsPyLValue Py.MPythonSig YPythonSig AddressT where
  ytrans (Py.IdentIsPyLValue t :&: _) = ytranslate t

instance YTrans Py.ExprIsRhs Py.MPythonSig YPythonSig ValueT where
  ytrans (Py.ExprIsRhs t :&: _) = ytranslate t

instance YTrans Py.ExprIsPositionalArgExp Py.MPythonSig YPythonSig ValueT where
  ytrans (Py.ExprIsPositionalArgExp t :&: _) = ytranslate t

instance YTrans Py.ExprIsFunctionExp Py.MPythonSig YPythonSig ValueT where
  ytrans (Py.ExprIsFunctionExp t :&: _) = ytranslate t

instance YTrans Py.FunctionCallIsExpr Py.MPythonSig YPythonSig ValueT where
  ytrans (Py.FunctionCallIsExpr t :&: l) = ytranslate t >>= (insertNode [l]) . ValF

instance YTrans Py.AssignIsStatement Py.MPythonSig YPythonSig StatementT where
  ytrans (Py.AssignIsStatement t :&: _) = ytranslate t >>= \(_ :: PyID MemValT) -> return Statement

ytransPythonModule :: (MonadYogoPy m) => TranslateM m Py.MPythonTermLab Py.ModuleL (ID YPythonSig ScopeT)
ytransPythonModule = ytranslate

initState :: YogoState YPythonSig
initState = YogoState [] [] Map.empty 0

fileToGraphPython :: Py.MPythonTermLab l -> YFile YPythonSig
fileToGraphPython t =
  let (a, state) = fromJust $ runIdentity $ runMaybeT $ runStateT ((onetdT $ promoteTF ytransPythonModule) t) initState in
    state ^. file

toGraphPython :: Project Py.MPythonSig -> YProject YPythonSig
toGraphPython = Map.map (\(E t) -> fileToGraphPython t)
