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

instance YTrans Py.Expr Py.MPythonSig YPythonSig ValueT where
  ytrans (Py.Int n _ _ :&: l) = makeNode [l] (ConstF (IntegerF n))
  ytrans f = error "Not Implemented"

instance YTrans Py.Statement Py.MPythonSig YPythonSig StatementT where
  ytrans (Py.StmtExpr expr _ :&: _) = ytranslate expr >>= \(_ :: PyID ValueT) -> return Statement
  ytrans f = error "Not Implemented"

instance YTrans Py.Module Py.MPythonSig YPythonSig ScopeT where
  ytrans (Py.Module body :&: _) = do
    newScope $ Name "Module"
    _ :: PyID [StatementT] <- ytranslate body
    return Scope

instance YTrans Py.IdentIsPyLValue Py.MPythonSig YPythonSig AddressT where
  ytrans (Py.IdentIsPyLValue t :&: _) = ytranslate t

instance YTrans Py.PyLhs Py.MPythonSig YPythonSig AddressT where
  ytrans (Py.PyLhs t :&: l) = do
    lvalues <- ytranslate t
    go (fromIds lvalues)
      where go [] = makeNode [] NothingF
            go ((id, occurrence) : xs) = go xs >>= (makeNode' occurrence) . (PyLhs id)

instance YTrans Py.ExprIsRhs Py.MPythonSig YPythonSig ValueT where
  ytrans (Py.ExprIsRhs t :&: _) = ytranslate t

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
