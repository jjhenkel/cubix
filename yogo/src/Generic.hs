{-# LANGUAGE TypeOperators #-}

module Generic (
  TermOcc,
  YGenericSig,
  YPythonSig,
  YPythonGraph,
  YFile,
  YProject,
  toGraphPython,
  ) where

import Data.Comp.Multi ( AnnTerm )
import Data.Comp.Multi.Ops ( (:+:) )
import Data.Comp.Multi.HFunctor ( E(..) )
import Data.Map ( Map )
import qualified Data.Map as Map

import Cubix.Language.Info
import Cubix.Language.Parametric.Syntax
import Cubix.Language.Python.Parametric.Common ( MPythonSig, MPythonTermLab, ModuleL )

newtype Occurrence = Occurrence [Label]
  deriving (Eq, Show)

type TermOcc f = AnnTerm Occurrence f

type YGenericSig = BoolF :+: IntF :+: IntegerF :+: CharF
type YPythonSig = YGenericSig
type YPythonGraph l = TermOcc YPythonSig l

-- A map from original source file path to a map of function graphs keyed by function name
newtype Name = Name String
type YFile f = Map Name (E (TermOcc f))
type YProject f = Map FilePath (YFile f)

fileToGraphPython :: MPythonTermLab ModuleL -> YFile YPythonSig
fileToGraphPython = error "what"

toGraphPython :: Project MPythonSig -> YProject YPythonSig
toGraphPython = const Map.empty
