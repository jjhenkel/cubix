{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Common.DSL (
  SigToLangDSL(..)
  ) where

import Data.Proxy ( Proxy(..) )
import Debug.Trace
import Data.Typeable

import Data.Comp.Multi
import Data.Map ( Map )
import qualified Data.Map as Map

import Common.Trans

type Namespace = String
type NodeType  = String
type ArgName   = String

class (Typeable f) => SigToLangDSL (f :: (* -> *) -> * -> *) where
  namespace :: Proxy f -> Namespace
  namespace = undefined

  nodeType :: Proxy f -> NodeType
  nodeType = undefined

  argNames :: Proxy f -> [ArgName]
  argNames = undefined

  derives :: Proxy f -> [(Namespace, NodeType)]
  derives = const []

  lang :: Proxy f -> Map Namespace [(NodeType, [ArgName], [(Namespace, NodeType)])]
  lang f = Map.singleton (namespace f) [(nodeType f, argNames f, derives f)]

instance (SigToLangDSL f1, SigToLangDSL f2) => SigToLangDSL (f1 :+: f2) where
  lang = const $ Map.unionWith (++) (lang (Proxy :: Proxy f1)) (lang (Proxy :: Proxy f2))

instance SigToLangDSL MemGenesisF where
  namespace = const "generic"
  nodeType = const "mem-genesis"
  argNames = const []
  derives = const []

instance SigToLangDSL ConstF where
  namespace = const "generic"
  nodeType = const "const"
  argNames = const ["$const"]
  derives = const []

primitiveToDSL :: Primitive -> String
primitiveToDSL (IntegerF n) = show n
primitiveToDSL (IntF n) = show n
primitiveToDSL (BoolF b) = if b then "true" else "false"
primitiveToDSL (StringF s) = s
