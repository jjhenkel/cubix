module Main where

import Control.Monad ( liftM, (<=<) )
import Data.Char  ( toLower )
import Data.Comp.Multi ( stripA )
import Data.Comp.Multi.Strategy.Classification ( dynProj )
import Data.Maybe ( fromJust )
import System.Environment ( getArgs )

import qualified Language.Dot.Pretty as Dot
import qualified Language.Dot.Syntax as Dot

import Cubix.ParsePretty
import Cubix.Language.Info
import Cubix.Language.Python.Parametric.Common as PCommon
import Cubix.Search.Search

data LangProj = PythonProj (Project MPythonSig)
data YLangProj = YPythonProj (YProject YPythonSig)

parseProj :: LabelGen -> String -> [FilePath] -> IO (Maybe LangProj)
parseProj gen "python"     = (return . maybe Nothing (Just . PythonProj)) <=< parseProject gen parseFile
parseProj _   _            = error "Unrecognized language. Must be one of: c, java, javascript, lua, python"

putProj :: LangProj -> IO ()
putProj (PythonProj p) = putProject (prettyPython     . fromJust . dynProj . stripA) p

lowercase :: String -> String
lowercase = map toLower

runYogo :: LangProj -> YLangProj
runYogo (PythonProj p) = YPythonProj . toGraphPython p

main = do
  gen <- mkCSLabelGen
  args <- getArgs
  let language = (lowercase (args !! 0))
  let langFiles = (args !! 1)
  let ruleFiles = (args !! 2)
  let sourceFiles = (drop 3 args)
  projRes <- parseProj gen language sourceFiles
  yogoProj <- case projRes of
                Nothing   -> error "Parse failed"
                Just proj -> runYogo proj

usage :: String
usage = "Usage:\n"
  ++ "search <language> \"<lang-file-1>,...\" \"<rule-file-1>,...\" <file>*\n"
