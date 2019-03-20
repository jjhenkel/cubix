{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Java.DSL (nsJava) where

import Data.Comp.Multi

import Common.DSL
import Common.Trans
import Java.Trans

instance SigToLangDSL JBlank where nodeDef _ = Just (nsJava, "blank", [], [])
instance NodeToGraphDSL JBlank YJavaSig where
  nodeArgs _ = []

nsJava :: Namespace
nsJava = "java"
