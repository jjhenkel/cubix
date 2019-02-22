module Cubix.Search.Search (
  TermOcc,
  YGenericSig,
  YPythonSig,
  YPythonGraph,
  YFile,
  YProject,
  toGraphPython,
  ) where

import Data.Comp.Multi ( AnnTerm )
import Data.Map ( Map )
import qualified Data.Map as Map

import Cubix.Language.Info ( Label )
import Cubix.Language.Parametric.Syntax.Base


newtype Occurrence = Occurrence [Labal]
  deriving (Eq, Show)

type TermOcc f = AnnTerm Occurrence f

type YGenericSig = BoolF :+: IntF :+: IntegerF :+: CharF
type YPythonSig = YGenericSig
type YPythonGraph l = TermOcc PythonSig l

-- A map from original source file path to a map of function graphs keyed by function name
newtype Name = Name String
type YFile = Map Name (E (TermOcc f))
type YProject f = Map FilePath YFile

fileToGraphPython :: MPythonTermLab -> YFile
fileToGraphPython

toGraphPython :: Project MPythonSig -> YProject YPythonSig
toGraphPython = Map.map $ fileToGraphPython . unE
