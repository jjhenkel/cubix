{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Common.DSL (
  Namespace
  , NodeType
  , ArgName
  , QualifiedNodeType
  , NodeDef
  , LangDefs

  , SigToLangDSL(..)
  , NodeToGraphDSL(..)
  , primitiveToDSL
  , idToDSL
  , occurrenceToDSL

  , nsCommon
  ) where

import Data.Proxy ( Proxy(..) )
import Debug.Trace
import Data.Typeable

import Cubix.Language.Info
import Data.Comp.Multi
import Data.List
import Data.Map ( Map )
import qualified Data.Map as Map

import Common.Trans

type Namespace = String
type NodeType  = String
type ArgName   = String
type QualifiedNodeType = String

type NodeDef   = (NodeType, [ArgName], [QualifiedNodeType])
type LangDefs  = Map Namespace [NodeDef]

nsCommon :: Namespace
nsCommon = "generic"

class (Typeable f) => SigToLangDSL (f :: (* -> *) -> * -> *) where
  namespace :: Proxy f -> Namespace
  nodeType :: Proxy f -> NodeType
  argNames :: Proxy f -> [ArgName]
  derives :: Proxy f -> [QualifiedNodeType]

  qualifiedNodeType :: Proxy f -> QualifiedNodeType
  qualifiedNodeType f = (namespace f) ++ "/" ++ (nodeType f)

  sigToDSL :: Proxy f -> LangDefs
  sigToDSL f = Map.singleton (namespace f) [(nodeType f, argNames f, derives f)]

instance (SigToLangDSL f1, SigToLangDSL f2) => SigToLangDSL (f1 :+: f2) where
  namespace = undefined
  nodeType = undefined
  argNames = undefined
  derives = undefined
  qualifiedNodeType = undefined

  sigToDSL = const $ Map.unionWith (++) (sigToDSL (Proxy :: Proxy f1)) (sigToDSL (Proxy :: Proxy f2))

instance SigToLangDSL MemGenesisF where
  namespace = const nsCommon
  nodeType = const "mem-genesis"
  argNames = const []
  derives = const []

instance SigToLangDSL ConstF where
  namespace = const nsCommon
  nodeType = const "const"
  argNames = const ["$const"]
  derives = const []

instance SigToLangDSL UnknownF where
  namespace = const nsCommon
  nodeType = const "unknown"
  argNames = const ["src"]
  derives = const []

instance SigToLangDSL MemF where
  namespace = const nsCommon
  nodeType = const "mem"
  argNames = const ["src"]
  derives = const []

----

class (SigToLangDSL f) => NodeToGraphDSL f y where
  nodeArgs :: f (ID y) t -> [String]

  nodeForm :: f (ID y) t -> (QualifiedNodeType, [String])
  nodeForm f = (qualifiedNodeType (Proxy :: Proxy f), nodeArgs f)

instance (NodeToGraphDSL f1 y, NodeToGraphDSL f2 y) => NodeToGraphDSL (f1 :+: f2) y where
  nodeArgs = caseH nodeArgs nodeArgs
  nodeForm = caseH nodeForm nodeForm

instance (MemGenesisF :<: y) => NodeToGraphDSL MemGenesisF y where
  nodeArgs = const []

instance (MemF :<: y) => NodeToGraphDSL MemF y where
  nodeArgs = const []

instance (ConstF :<: y) => NodeToGraphDSL ConstF y where
  nodeArgs (ConstF p) = [primitiveToDSL p]

instance (UnknownF :<: y) => NodeToGraphDSL UnknownF y where
  nodeArgs (UnknownF mem) = [idToDSL mem]

primitiveToDSL :: Primitive -> String
primitiveToDSL (IntegerF n) = show n
primitiveToDSL (IntF n) = show n
primitiveToDSL (BoolF b) = if b then "true" else "false"
primitiveToDSL (StringF s) = s

idToDSL :: ID f t -> String
idToDSL (ID n) = "n-" ++ show n
idToDSL t = error $ "id \"" ++ show t ++ "\" cannot be translated to DSL"

occurrenceToDSL :: Occurrence -> String
occurrenceToDSL (Occurrence labels) = "[" ++ (intercalate " " $ map (\l -> "\"" ++ show l ++ "\"") labels) ++ "]"
