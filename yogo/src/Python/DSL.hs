{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Python.DSL (nsPy) where

import Data.Comp.Multi

import Common.DSL
import Common.Trans
import Python.Trans

nsPy :: Namespace
nsPy = "python"

instance SigToLangDSL ParenLValue where
  namespace = const nsPy
  nodeType = const "paren-lvalue"
  argNames = const ["lvalue"]
  derives = const ["engine/any-lvalue"]

instance (ParenLValue :<: y) => NodeToGraphDSL ParenLValue y where
  nodeArgs (ParenLValue lvalue) = [idToDSL lvalue]
