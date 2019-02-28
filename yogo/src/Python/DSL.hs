{-# LANGUAGE TypeOperators #-}

module Python.DSL (
  m
  ) where

import Data.Proxy ( Proxy(..) )
import Data.Comp.Multi

import Common.DSL
import Common.Trans
import Python.Trans

instance SigToLangDSL ParenLValue where
  namespace = const "python"
  nodeType = const "paren-lvalue"
  argNames = const ["lvalue"]
  derives = const [("engine", "any-lvalue")]

m = lang $ (Proxy :: Proxy (ParenLValue :+: ConstF :+: MemGenesisF))
