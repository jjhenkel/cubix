{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Python.DSL () where

import Data.Comp.Multi

import Common.DSL
import Common.Trans
import Python.Trans

instance SigToLangDSL ParenLValue where
  namespace = const "python"
  nodeType = const "paren-lvalue"
  argNames = const ["lvalue"]
  derives = const ["engine/any-lvalue"]

instance (ParenLValue :<: y) => NodeToGraphDSL ParenLValue y where
  nodeArgs (ParenLValue lvalue) = [idToDSL lvalue]
