{-# LANGUAGE ConstraintKinds #-}
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

import Control.Monad.Identity ( Identity(..), runIdentity )
import Data.Comp.Multi ( AnnTerm, inject', (:&:)(..) )
import Data.Comp.Multi.Ops ( (:+:) )
import Data.Comp.Multi.HFunctor ( E(..), (:=>) )
import Data.Comp.Multi.Strategic
import Data.Map ( Map )
import qualified Data.Map as Map

import Cubix.Language.Info
import Cubix.Language.Parametric.Syntax
import Cubix.Language.Python.Parametric.Common ( MPythonSig, MPythonTermLab, ModuleL )

newtype Occurrence = Occurrence [Label]
  deriving (Eq, Show)

type TermOcc f = AnnTerm Occurrence f

type YGenericSig = BoolF :+: IntF :+: IntegerF :+: CharF

-- A map from original source file path to a map of function graphs keyed by function name
newtype Name = Name [Char]
  deriving (Eq, Show, Ord)

type YFile f = Map Name (E (TermOcc f))
type YProject f = Map FilePath (YFile f)

type MonadYogo m = (Monad m)

type YPythonSig = YGenericSig
type YPythonGraph l = TermOcc YPythonSig l
type YPythonFile = YFile YPythonSig

fileToGraphPython' :: (MonadYogo m) => TranslateM m MPythonTermLab ModuleL YPythonFile
fileToGraphPython' = const $ return $ Map.singleton (Name "fn1") (E $ inject' $ (IntF 1) :&: Occurrence [])

-- Note: runIdentity is how haskell knows that monad type. 'head' works too, will just make it a list
fileToGraphPython :: MPythonTermLab :=> YPythonFile
fileToGraphPython = runIdentity . (crushtdT $ promoteTF $ addFail fileToGraphPython')

toGraphPython :: Project MPythonSig -> YProject YPythonSig
toGraphPython = Map.map (\(E t) -> fileToGraphPython t)
