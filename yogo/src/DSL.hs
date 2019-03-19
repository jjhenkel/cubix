module DSL (
  Namespace
  , generateLangFiles
  , generateGraphFiles

  , nsCommon
  , nsPy
  , nsJava
  ) where

import Data.Proxy ( Proxy(..) )
import Debug.Trace
import Data.Typeable

import Data.Comp.Multi
import Data.List
import Data.Map ( Map )
import qualified Data.Map as Map

import Common.Trans
import Common.DSL
import Python.DSL
import Java.DSL

generateNodeLine :: NodeDef -> String
generateNodeLine (_, node, args, derives) = "(defnode " ++ node ++ " [" ++ (intercalate " " args) ++ "] " ++ (intercalate " " derives) ++ ")"

generateLangFile :: Namespace -> [NodeDef] -> String
generateLangFile ns nodes = "(ns " ++ ns ++ ")\n\n" ++ (intercalate "\n" (map generateNodeLine nodes))

generateLangFiles :: (SigToLangDSL sig) => Proxy sig -> Map Namespace String
generateLangFiles proxy = Map.mapWithKey generateLangFile (sigToDSL proxy)

-- -----------
-- Graph files
-- -----------

generateGraphLine :: (NodeToGraphDSL f f) => YGraphEntry f -> String
generateGraphLine (YGraphNode (E id) (E (Node node)) occurrence) =
  case nodeForm node of
    Left form -> idToDSL id ++ " " ++ form
    Right (fn, args) -> idToDSL id ++ " (" ++ fn ++ " " ++ (intercalate " " args) ++ " " ++ occurrenceToDSL occurrence ++ ")"
generateGraphLine (YGraphEq (E id1) (E id2)) = "_ (s/make-eq " ++ idToDSL id1 ++ " " ++ idToDSL id2 ++ ")"

generateGraph :: (NodeToGraphDSL f f) => Name -> YGraph f -> String
generateGraph (Name name) graph =
  "(defgraph g-" ++ name ++ "\n  " ++ (intercalate "\n  " $ reverse $ map generateGraphLine graph) ++ ")\n"

generateGraphFile :: (NodeToGraphDSL f f) => Namespace -> YFile f -> String
generateGraphFile ns graphs =
  "(ns " ++ ns ++ ")\n\n" ++ (Map.foldlWithKey (\s k g -> s ++ "\n" ++ (generateGraph k g)) "" graphs)

generateGraphFiles :: (NodeToGraphDSL f f) => YProject f -> Map FilePath (Namespace, String)
generateGraphFiles = (foldl (\m (i, (p, f)) ->
                               let ns = "graph" ++ show i in
                                 Map.insert p (ns, generateGraphFile ns f) m) Map.empty) . (zip [0..]) . Map.toList
