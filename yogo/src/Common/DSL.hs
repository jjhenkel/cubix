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
  , anyMem
  , anyLValue
  , anyStackLValue
  , anyHeapLValue
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

type NodeDef   = (Namespace, NodeType, [ArgName], [QualifiedNodeType])
type LangDefs  = Map Namespace [NodeDef]

nsCommon :: Namespace
nsCommon = "generic"

-- Here for completion. We don't generate engine file.
nsEngine :: Namespace
nsEngine = "engine"

qualifiedNodeType :: (SigToLangDSL f) => Proxy f -> QualifiedNodeType
qualifiedNodeType f = let (ns, typ, _, _) = nodeDef f in ns ++ "/" ++ typ

nsOf :: (SigToLangDSL f) => Proxy f -> Namespace
nsOf f = let (ns, _, _, _) = nodeDef f in ns

class (Typeable f) => SigToLangDSL (f :: (* -> *) -> * -> *) where
  nodeDef :: Proxy f -> NodeDef

  sigToDSL :: Proxy f -> LangDefs
  sigToDSL f = Map.singleton (nsOf f) [nodeDef f]

instance (SigToLangDSL f1, SigToLangDSL f2) => SigToLangDSL (f1 :+: f2) where
  nodeDef = undefined
  sigToDSL _ = Map.unionWith (++) (sigToDSL (Proxy :: Proxy f1)) (sigToDSL (Proxy :: Proxy f2))

anyMem :: QualifiedNodeType
anyMem = qualifiedNodeType (Proxy :: Proxy AnyMem)

anyLValue :: QualifiedNodeType
anyLValue = qualifiedNodeType (Proxy :: Proxy AnyLValue)

anyStackLValue :: QualifiedNodeType
anyStackLValue = qualifiedNodeType (Proxy :: Proxy AnyStackF)

anyHeapLValue :: QualifiedNodeType
anyHeapLValue = qualifiedNodeType (Proxy :: Proxy AnyHeapF)

-- Engine Nodes

instance SigToLangDSL AnyNode where nodeDef _ = (nsEngine, "any-node", [], [])
instance SigToLangDSL AnyLValue where nodeDef _ = (nsEngine, "any-lvalue", [], [])
instance SigToLangDSL AnyMem where nodeDef _ = (nsEngine, "any-mem", [], [])
instance SigToLangDSL Q where nodeDef _ = (nsEngine, "q", [], [])

-- Generic Nodes

instance SigToLangDSL AnyStackF where nodeDef _ = (nsCommon, "any-stack-lvalue", [], [anyLValue])
instance SigToLangDSL AnyHeapF where nodeDef _ = (nsCommon, "any-heap-lvalue", [], [anyLValue])
instance SigToLangDSL NothingF where nodeDef _ = (nsCommon, "nothing", [], [])
instance SigToLangDSL ConstF where nodeDef _ = (nsCommon, "const", ["$const"], [])
instance SigToLangDSL IdentF where nodeDef _ = (nsCommon, "ident", ["$name"], [anyStackLValue])
instance SigToLangDSL AssignF where nodeDef _ = (nsCommon, "assign", ["mem", "lvalue", "rvalue"], [])
instance SigToLangDSL MemGenesisF where nodeDef _ = (nsCommon, "mem-genesis", [], [anyMem])
instance SigToLangDSL UnknownF where nodeDef _ = (nsCommon, "unknown", ["src"], [anyMem])
instance SigToLangDSL MemF where nodeDef _ = (nsCommon, "mem", ["src"], [anyMem])

----

class (SigToLangDSL f) => NodeToGraphDSL f y where
  nodeArgs :: f (ID y) t -> [String]

  nodeForm :: f (ID y) t -> (QualifiedNodeType, [String])
  nodeForm f = (qualifiedNodeType (Proxy :: Proxy f), nodeArgs f)

instance (NodeToGraphDSL f1 y, NodeToGraphDSL f2 y) => NodeToGraphDSL (f1 :+: f2) y where
  nodeArgs = caseH nodeArgs nodeArgs
  nodeForm = caseH nodeForm nodeForm

instance (MemGenesisF :<: y) => NodeToGraphDSL MemGenesisF y where
  nodeArgs _ = []

instance (MemF :<: y) => NodeToGraphDSL MemF y where
  nodeArgs _ = []

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
