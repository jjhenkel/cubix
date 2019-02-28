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
  , ParenLValue(..)
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

data ParenLValue e l where
  ParenLValue :: e AddressT -> ParenLValue e AddressT

type YPythonSig = ParenLValue :+: YGenericSig
type PyID t = ID YPythonSig t
type YTranslatePyM m f t = GTranslateM m ((f :&: Label) Py.MPythonTermLab) (ID YPythonSig t)

type MonadYogoPy m = (MonadYogo YPythonSig m)
type CanYTransPy f = (CanYTrans f)

instance YTrans Py.Expr Py.MPythonSig YPythonSig ValueT where
  ytrans ((Py.Int n _ _) :&: label) = makeNode (ConstF (IntegerF n)) [label]
  ytrans f = error "Not Implemented"

instance YTrans Py.Statement Py.MPythonSig YPythonSig StatementT where
  ytrans ((Py.StmtExpr expr _) :&: _) = do
    traceM "StmtExpr"
    _ :: PyID ValueT <- ytranslate expr
    return Statement
  ytrans f = error "Not Implemented"

instance YTrans Py.Module Py.MPythonSig YPythonSig ScopeT where
  ytrans ((Py.Module body) :&: _) = do
    traceM "module"
    id <- getNextID
    let name = Name "Module"
    nameScope %= (name :)
    memScope  %= (id :)
    file .= Map.singleton name []
    _ :: PyID [StatementT] <- ytranslate body
    f <- use file
    traceM $ show $ f
    return Scope

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
