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

instance (PyLhs :<: y) => NodeToGraphDSL PyLhs y where
  nodeArgs (PyLhs id1 id2) = [idToDSL id1, idToDSL id2]
