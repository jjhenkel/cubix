{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad ( liftM, (<=<) )
import Data.Char  ( toLower )
import Data.Comp.Multi
import Data.Comp.Multi.Strategy.Classification ( dynProj )
import Data.Maybe ( fromJust )
import Data.Map ( Map(..) )
import qualified Data.Map as Map
import System.Environment ( getArgs )

import Cubix.Language.Info
import Cubix.ParsePretty
import Cubix.Language.Python.Parametric.Common as PCommon

import Common.Trans
import Python.Trans as Py
import DSL

import Data.Proxy ( Proxy(..) )

data LangProj = PythonProj (Project MPythonSig)
data YLangProj = YPythonProj (YProject Py.YPythonSig)

parseProj :: LabelGen -> String -> [FilePath] -> IO (Maybe LangProj)
parseProj gen "python"     = (return . maybe Nothing (Just . PythonProj)) <=< parseProject gen parseFile
parseProj _   _            = error "Unrecognized language. Must be one of: c, java, javascript, lua, python"

putProj :: LangProj -> IO ()
putProj (PythonProj p) = putProject (prettyPython     . fromJust . dynProj . stripA) p

lowercase :: String -> String
lowercase = map toLower

runYogo :: LangProj -> YLangProj
runYogo (PythonProj p) = YPythonProj (Py.toGraphPython p)

writeTmp :: FilePath -> String -> IO FilePath
writeTmp f s = do
  let f' = "/tmp/" ++ f
  writeFile f' s
  return f'

projToFiles :: YLangProj -> IO (Map FilePath FilePath)
projToFiles prj = do
  let graphFiles = (case prj of
                      YPythonProj p -> generateGraphFiles p)
  mapM (\(k, (f, s)) -> writeTmp f s >>= return . ((,) k)) (Map.toList graphFiles) >>= return . Map.fromList

m = (Proxy :: Proxy (Py.ParenLValue :+: ConstF :+: MemGenesisF))

main = do
  gen <- mkCSLabelGen
  args <- getArgs
  let language = (lowercase (args !! 0))
  let langFiles = (args !! 1)
  let ruleFiles = (args !! 2)
  let sourceFiles = (drop 3 args)
  projRes <- parseProj gen language sourceFiles
  let yogoProj = case projRes of
                   Nothing   -> error "Parse failed"
                   Just proj -> runYogo proj
  putStrLn $ fromJust $ Map.lookup "python" (generateLangFiles m)
  projToFiles yogoProj

usage :: String
usage = "Usage:\n"
  ++ "search <language> \"<lang-file-1>,...\" \"<rule-file-1>,...\" <file>*\n"
