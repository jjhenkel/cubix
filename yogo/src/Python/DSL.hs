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

instance SigToLangDSL PyLhs where nodeDef _ = Just (nsPy, "lhs", ["lv1", "lv2"], [anyLValue])
instance SigToLangDSL PyListLV where nodeDef _ = Just (nsPy, "list-lv", ["lv", "lvs"], [anyLValue])
instance SigToLangDSL PyArgKeyword where nodeDef _ = Just (nsPy, "arg-kw", ["param", "val"], [])
instance SigToLangDSL PyArgSplat where nodeDef _ = Just (nsPy, "args", ["val"], [])
instance SigToLangDSL PyArgKWSplat where nodeDef _ = Just (nsPy, "kwargs", ["val"], [])
instance SigToLangDSL PyTuple where nodeDef _ = Just (nsPy, "tuple", ["head", "rest"], [])
instance SigToLangDSL PyList where nodeDef _ = Just (nsPy, "list", ["head", "rest"], [])
instance SigToLangDSL PyDict where nodeDef _ = Just (nsPy, "dict", ["$uid", "depth"], [])
instance SigToLangDSL PySet where nodeDef _ = Just (nsPy, "set", ["$uid", "depth"], [])
instance SigToLangDSL PyOp where nodeDef _ = Nothing

instance NodeToGraphDSL PyLhs YPythonSig where
  nodeArgs (PyLhs id1 id2) = [idToDSL id1, idToDSL id2]

instance NodeToGraphDSL PyListLV YPythonSig where
  nodeArgs (PyListLV id1 id2) = [idToDSL id1, idToDSL id2]

instance NodeToGraphDSL PyOp YPythonSig where
  nodeArgs _ = []
  nodeForm (PyOp op) = Left $ pyOpToDSL op

instance NodeToGraphDSL PyArgKeyword YPythonSig where
  nodeArgs (PyArgKeyword param val) = [idToDSL param, idToDSL val]

instance NodeToGraphDSL PyArgSplat YPythonSig where
  nodeArgs (PyArgSplat val) = [idToDSL val]

instance NodeToGraphDSL PyArgKWSplat YPythonSig where
  nodeArgs (PyArgKWSplat val) = [idToDSL val]

instance NodeToGraphDSL PyTuple YPythonSig where
  nodeArgs (PyTuple head rest) = [idToDSL head, idToDSL rest]

instance NodeToGraphDSL PyList YPythonSig where
  nodeArgs (PyList head rest) = [idToDSL head, idToDSL rest]

instance NodeToGraphDSL PySet YPythonSig where
  nodeArgs (PySet head rest) = [idToDSL head, idToDSL rest]

instance NodeToGraphDSL PyDict YPythonSig where
  nodeArgs (PyDict key value rest) = [idToDSL key, idToDSL value, idToDSL rest]


pyOpToDSL :: PyOp' -> String
pyOpToDSL PyIn = ":py-in"
pyOpToDSL PyNotIn = ":py-not-in"
