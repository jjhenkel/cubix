{-# LANGUAGE TypeOperators #-}

module Main where

import Control.Monad ( liftM, (<=<) )
import Control.Concurrent.STM (atomically)
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Char  ( toLower )
import Data.Comp.Multi
import Data.Comp.Multi.Strategy.Classification ( dynProj )
import Data.List ( intercalate )
import Data.Maybe ( fromJust )
import Data.Map ( Map(..) )
import qualified Data.Map as Map
import System.Environment ( getArgs )
import System.Process.Typed

import Cubix.Language.Info
import Cubix.ParsePretty
import Cubix.Language.Java.Parametric.Common as JCommon
import Cubix.Language.Python.Parametric.Common as PCommon

import Common.Trans
import Python.Trans as Py
import Java.Trans as J
import DSL

import Data.Proxy ( Proxy(..) )

data LangProj = PythonProj (Project MPythonSig)
              | JavaProj (Project MJavaSig)
data YLangProj = YPythonProj (YProject Py.YPythonSig)
               | YJavaProj (YProject J.YJavaSig)

parseProj :: LabelGen -> String -> [FilePath] -> IO (Maybe LangProj)
parseProj gen "python"     = (return . maybe Nothing (Just . PythonProj)) <=< parseProject gen parseFile
parseProj gen "java"     = (return . maybe Nothing (Just . JavaProj)) <=< parseProject gen parseFile
parseProj _   _            = error "Unrecognized language. Must be one of: c, java, javascript, lua, python"

putProj :: LangProj -> IO ()
putProj (PythonProj p) = putProject (prettyPython     . fromJust . dynProj . stripA) p
putProj (JavaProj   p) = putProject (prettyJava       . fromJust . dynProj . stripA) p

lowercase :: String -> String
lowercase = map toLower

runYogo :: LangProj -> YLangProj
runYogo (PythonProj p) = YPythonProj (Py.toGraphPython p)
runYogo (JavaProj p) = YJavaProj (J.toGraphJava p)

writeTmp :: FilePath -> String -> IO FilePath
writeTmp f s = do
  let f' = "/tmp/" ++ f
  writeFile f' s
  return f'

-- Return a map from source file paths to graph file paths
projToFiles :: YLangProj -> IO (Map FilePath FilePath)
projToFiles prj = do
  let graphFiles = (case prj of
                      YPythonProj p -> generateGraphFiles p
                      YJavaProj   p -> generateGraphFiles p)
  mapM (\(k, (f, s)) -> writeTmp f s >>= return . ((,) k)) (Map.toList graphFiles) >>= return . Map.fromList

writeLangFiles :: Namespace -> Map Namespace String -> IO [FilePath]
writeLangFiles ns langFiles = do
  fileLang <- writeTmp ("lang-" ++ ns) (fromJust $ Map.lookup ns langFiles)
  fileGeneric <- writeTmp "lang-generic" (fromJust $ Map.lookup "generic" langFiles)
  return [fileGeneric, fileLang]

langToFiles :: String -> IO [FilePath]
langToFiles "python" = writeLangFiles nsPy   $ generateLangFiles (Proxy :: Proxy Py.YPythonSig)
langToFiles "java"   = writeLangFiles nsJava $ generateLangFiles (Proxy :: Proxy J.YJavaSig)
langToFiles _ = error "Unsupported language"


yogoCommand' = ["java", "-jar" ,"/home/pondpremt/matcher-prot/target/uberjar/matcher-prot-0.1.0-SNAPSHOT-standalone.jar"]

main = do
  gen <- mkCSLabelGen
  args <- getArgs
  let language = (lowercase (args !! 0))
  let additionalLangFiles = args !! 1
  let ruleFiles = (args !! 2)
  let sourceFiles = (drop 3 args)
  projRes <- parseProj gen language sourceFiles
  let yogoProj = case projRes of
                   Nothing   -> error "Parse failed"
                   Just proj -> runYogo proj
  langFiles <- langToFiles language
  graphFiles <- projToFiles yogoProj

  let langFilesArg = ((intercalate "," langFiles) ++ case additionalLangFiles of
                                                               "" -> ""
                                                               x -> "," ++ x)
  let ruleFilesArg = ruleFiles
  let graphFilesArg = intercalate "," (Map.elems graphFiles)

  let yogoCommand = yogoCommand' ++ ["-l", langFilesArg, "-r", ruleFilesArg, "-g", graphFilesArg]
  print $ "yogoCommand " ++ show yogoCommand
  let catConfig = setStdin createPipe
                $ setStdout byteStringOutput
                $ proc (head yogoCommand) (tail yogoCommand)
  withProcess_ catConfig $ \p -> do
    atomically (getStdout p) >>= L8.putStrLn


usage :: String
usage = "Usage:\n"
  ++ "search <language> \"<lang-file-1>,...\" \"<rule-file-1>,...\" <file>*\n"
