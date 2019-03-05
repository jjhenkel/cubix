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
import Data.Maybe ( fromJust )

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
qualifiedNodeType f = let (ns, typ, _, _) = fromJust (nodeDef f) in ns ++ "/" ++ typ

nsOf :: (SigToLangDSL f) => Proxy f -> Namespace
nsOf f = let (ns, _, _, _) = fromJust (nodeDef f) in ns

anyMem :: QualifiedNodeType
anyMem = qualifiedNodeType (Proxy :: Proxy AnyMem)

anyLValue :: QualifiedNodeType
anyLValue = qualifiedNodeType (Proxy :: Proxy AnyLValue)

anyStackLValue :: QualifiedNodeType
anyStackLValue = qualifiedNodeType (Proxy :: Proxy AnyStack)

anyHeapLValue :: QualifiedNodeType
anyHeapLValue = qualifiedNodeType (Proxy :: Proxy AnyHeap)

class (Typeable f) => SigToLangDSL (f :: (* -> *) -> * -> *) where
  nodeDef :: Proxy f -> Maybe NodeDef

  sigToDSL :: Proxy f -> LangDefs
  sigToDSL f = case nodeDef f of
                 Just def -> Map.singleton (nsOf f) [def]
                 Nothing -> Map.empty

instance (SigToLangDSL f1, SigToLangDSL f2) => SigToLangDSL (f1 :+: f2) where
  nodeDef _ = Nothing
  -- Signature is defined backward
  sigToDSL _ = Map.unionWith (++) (sigToDSL (Proxy :: Proxy f2)) (sigToDSL (Proxy :: Proxy f1))

-- Engine Nodes. Not generated

instance SigToLangDSL AnyNode where nodeDef _ = Just (nsEngine, "any-node", [], [])
instance SigToLangDSL AnyLValue where nodeDef _ = Just (nsEngine, "any-lvalue", [], [])
instance SigToLangDSL AnyMem where nodeDef _ = Just (nsEngine, "any-mem", [], [])
instance SigToLangDSL Q where nodeDef _ = Just (nsEngine, "q", [], [])

-- Non-nodes

instance SigToLangDSL CommonOp where nodeDef _ = Nothing

-- Generic Nodes

instance SigToLangDSL AnyStack where nodeDef _ = Just (nsCommon, "any-stack-lvalue", [], [anyLValue])
instance SigToLangDSL AnyHeap where nodeDef _ = Just (nsCommon, "any-heap-lvalue", [], [anyLValue])
instance SigToLangDSL MemGenesisF where nodeDef _ = Just (nsCommon, "mem-genesis", [], [anyMem])
instance SigToLangDSL UnknownF where nodeDef _ = Just (nsCommon, "unknown", ["src"], [anyMem])
instance SigToLangDSL MemF where nodeDef _ = Just (nsCommon, "mem", ["src"], [anyMem])
instance SigToLangDSL ValF where nodeDef _ = Just (nsCommon, "val", ["src"], [])
instance SigToLangDSL NothingF where nodeDef _ = Just (nsCommon, "nothing", [], [])
instance SigToLangDSL ConstF where nodeDef _ = Just (nsCommon, "const", ["$const"], [])
instance SigToLangDSL IdentF where nodeDef _ = Just (nsCommon, "ident", ["$name"], [anyStackLValue])
instance SigToLangDSL SelF where nodeDef _ = Just (nsCommon, "sel", ["base", "offset"], [])
instance SigToLangDSL BinOpF where nodeDef _ = Just (nsCommon, "binop", ["$op", "arg1", "arg2"], [])
instance SigToLangDSL UnOpF where nodeDef _ = Just (nsCommon, "unop", ["$op", "arg"], [])
instance SigToLangDSL AssignF where nodeDef _ = Just (nsCommon, "assign", ["mem", "lvalue", "rvalue"], [])
instance SigToLangDSL FunctionCallF where nodeDef _ = Just (nsCommon, "fcall", ["mem", "f", "args"], [])
instance SigToLangDSL FunctionArgsF where nodeDef _ = Just (nsCommon, "fargs", ["arg", "args"], [])

----

class (SigToLangDSL f) => NodeToGraphDSL f y where
  nodeArgs :: f (ID y) t -> [String]

  nodeForm :: f (ID y) t -> Either String (QualifiedNodeType, [String])
  nodeForm f = Right (qualifiedNodeType (Proxy :: Proxy f), nodeArgs f)

instance (NodeToGraphDSL f1 y, NodeToGraphDSL f2 y) => NodeToGraphDSL (f1 :+: f2) y where
  nodeArgs = caseH nodeArgs nodeArgs
  nodeForm = caseH nodeForm nodeForm

instance (AnyNode :<: y) => NodeToGraphDSL AnyNode y where
  nodeArgs _ = error "AnyNode should not be an explicit node"

instance (AnyLValue :<: y) => NodeToGraphDSL AnyLValue y where
  nodeArgs _ = error "AnyLValue should not be an explicit node"

instance (AnyMem :<: y) => NodeToGraphDSL AnyMem y where
  nodeArgs _ = error "AnyMem should not be an explicit node"

instance (AnyStack :<: y) => NodeToGraphDSL AnyStack y where
  nodeArgs _ = error "AnyStack should not be an explicit node"

instance (AnyHeap :<: y) => NodeToGraphDSL AnyHeap y where
  nodeArgs _ = error "AnyHeap should not be an explicit node"

instance (Q :<: y) => NodeToGraphDSL Q y where
  nodeArgs (Q lvalue mem) = [idToDSL mem, idToDSL lvalue]

instance (CommonOp :<: y) => NodeToGraphDSL CommonOp y where
  nodeArgs _ = []
  nodeForm (CommonOp op) = Left $ commonOpToDSL op

instance (MemGenesisF :<: y) => NodeToGraphDSL MemGenesisF y where
  nodeArgs _ = []

instance (MemF :<: y) => NodeToGraphDSL MemF y where
  nodeArgs (MemF src) = [idToDSL src]

instance (ValF :<: y) => NodeToGraphDSL ValF y where
  nodeArgs (ValF src) = [idToDSL src]

instance (UnknownF :<: y) => NodeToGraphDSL UnknownF y where
  nodeArgs (UnknownF mem) = [idToDSL mem]

instance (NothingF :<: y) => NodeToGraphDSL NothingF y where
  nodeArgs _ = []

instance (ConstF :<: y) => NodeToGraphDSL ConstF y where
  nodeArgs (ConstF p) = [primitiveToDSL p]

instance (IdentF :<: y) => NodeToGraphDSL IdentF y where
  nodeArgs (IdentF name) = [quoteStr name]

instance (SelF :<: y) => NodeToGraphDSL SelF y where
  nodeArgs (SelF base offset) = [idToDSL base, idToDSL offset]

instance (BinOpF :<: y) => NodeToGraphDSL BinOpF y where
  nodeArgs (BinOpF op arg1 arg2) = [idToDSL op, idToDSL arg1, idToDSL arg2]

instance (UnOpF :<: y) => NodeToGraphDSL UnOpF y where
  nodeArgs (UnOpF op arg) = [idToDSL op, idToDSL arg]

instance (AssignF :<: y) => NodeToGraphDSL AssignF y where
  nodeArgs (AssignF rvalue lvalue mem) = [idToDSL mem, idToDSL lvalue, idToDSL rvalue]

instance (FunctionCallF :<: y) => NodeToGraphDSL FunctionCallF y where
  nodeArgs (FunctionCallF f args mem) = [idToDSL mem, idToDSL f, idToDSL args]

instance (FunctionArgsF :<: y) => NodeToGraphDSL FunctionArgsF y where
  nodeArgs (FunctionArgsF arg args) = [idToDSL arg, idToDSL args]

quoteStr :: String -> String
quoteStr s = "\"" ++ s ++ "\""

primitiveToDSL :: Primitive -> String
primitiveToDSL (IntegerF n) = show n
primitiveToDSL (IntF n) = show n
primitiveToDSL (BoolF b) = if b then "true" else "false"
primitiveToDSL (StringF s) = quoteStr s

idToDSL :: ID f t -> String
idToDSL (ID n) = "n-" ++ show n
idToDSL t = error $ "id \"" ++ show t ++ "\" cannot be translated to DSL"

occurrenceToDSL :: Occurrence -> String
occurrenceToDSL (Occurrence labels) = "[" ++ (intercalate " " $ map (\l -> quoteStr $ show l) labels) ++ "]"

commonOpToDSL :: CommonOp' -> String
commonOpToDSL And = ":and"
commonOpToDSL Or = ":or"
commonOpToDSL Not = ":not"
commonOpToDSL Exponent = ":**"
commonOpToDSL Plus = ":+"
commonOpToDSL Minus = ":-"
commonOpToDSL Multiply = ":*"
commonOpToDSL LessThan = ":<"
commonOpToDSL GreaterThan = ":>"
commonOpToDSL Equals = ":=="
commonOpToDSL LessThanEquals = ":<="
commonOpToDSL GreaterThanEquals = ":>="
commonOpToDSL NotEquals = ":!="
