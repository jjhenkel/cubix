{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Environment ( getArgs )

import Data.Aeson
import Data.ByteString.Lazy.UTF8 ( toString )

import Control.Monad ( MonadPlus(..), liftM, (<=<), (>=>) )
import Control.Monad.Identity ( Identity(..) )
import Control.Monad.Trans ( lift )
import Control.Monad.Trans.Maybe ( MaybeT(..) )
import Control.Monad.Writer ( MonadWriter(..), Writer, runWriter, execWriterT)
import Data.Typeable ( Typeable )

import Language.Java.Pretty ( prettyPrint )

import Data.Comp.Multi
import Data.Comp.Multi.Strategic

import           Cubix.Language.Info
import           Cubix.Language.Java.Parametric.Full
import qualified Cubix.Language.Java.Parametric.Common as Common
import qualified Cubix.Language.Java.Parse as Parse

parseFile :: FilePath -> IO (Maybe (JavaTerm CompilationUnitL))
parseFile path = do
  res <- Parse.parse path
  case res of
    Left  x -> return Nothing
    Right p -> return $ Just $ translate p


vandalize' :: Rewrite JavaTerm IdentL
vandalize' (project -> (Just (Ident s))) = return $ iIdent (s ++ "Counter")

vandalize :: GRewrite JavaTerm
vandalize = allbuR $ promoteR $ addFail vandalize'

emptyBlockStmt :: JavaTerm BlockStmtL
emptyBlockStmt = iBlockStmt iEmpty

isEmptyBlockStmt :: JavaTerm BlockStmtL -> Bool
isEmptyBlockStmt = (== emptyBlockStmt)

removeWhiles' :: (MonadPlus m) => RewriteM m JavaTerm StmtL
removeWhiles' (project -> (Just (While _ _))) = return $ iEmpty
removeWhiles' _                               = mzero

liftBlockStmt :: (MonadPlus m) => RewriteM m JavaTerm StmtL -> RewriteM m JavaTerm BlockStmtL
liftBlockStmt r (project -> Just (BlockStmt s)) = liftM iBlockStmt (r s)
liftBlockStmt r _                               = mzero

emptyIsDelete :: (f ~ JavaTerm, Common.ExtractF [] f, Common.InsertF [] f, Monad m, Typeable i) => (f i -> Bool) -> RewriteM m f i -> RewriteM m f [i]
emptyIsDelete isEmpty r term@(Common.extractF -> (t : ts)) = do
  rewritten <- r t
  return $ if isEmpty t then
             Common.insertF (rewritten : ts)
           else if isEmpty rewritten then
             Common.insertF ts
           else
             Common.insertF (rewritten : ts)
emptyIsDelete isEmpty r (Common.extractF -> []) = return $ Common.insertF []

removeWhiles :: GRewrite JavaTerm
removeWhiles = tryR $ anytdR $ promoteRF $ emptyIsDelete isEmptyBlockStmt $ liftBlockStmt removeWhiles'

data DeclSpec = DeclSpec (JavaTerm [ModifierL]) (JavaTerm TypeL) (JavaTerm VarDeclIdL)
  deriving ( Eq, Ord, Show )

getDeclId :: JavaTerm VarDeclL -> JavaTerm VarDeclIdL
getDeclId (project -> Just (VarDecl id _)) = id

collectLocalVars :: (MonadWriter [DeclSpec] m) => TranslateM m JavaTerm BlockStmtL ()
collectLocalVars (project -> (Just (LocalVars mods typ decls))) = tell (map (DeclSpec mods typ) $ map getDeclId $ Common.extractF decls)
collectLocalVars _                                              = return ()

collectDecls :: (MonadWriter [DeclSpec] m) => GTranslateM m JavaTerm ()
collectDecls = mtryM . (promoteTF $ addFail collectLocalVars)

varDeclIdToLhs :: JavaTerm VarDeclIdL -> JavaTerm LhsL
varDeclIdToLhs (project -> Just (VarId id)) = iNameLhs $ iName $ Common.insertF [id]
varDeclIdToLhs (project -> Just (VarDeclArray inner)) = varDeclIdToLhs inner

varDeclArrayDim :: JavaTerm VarDeclIdL -> Int
varDeclArrayDim (project -> Just (VarId id))          = 0
varDeclArrayDim (project -> Just (VarDeclArray inner)) = 1 + varDeclArrayDim inner

declToAssn :: JavaTerm TypeL -> JavaTerm VarDeclL -> [JavaTerm ExpL]
declToAssn _ (project -> Just (VarDecl _ (Common.extractF -> Nothing)))    = []
declToAssn t (project -> Just (VarDecl id (Common.extractF -> Just init))) =
  case project init of
    Just (InitExp e)     -> [iAssign (varDeclIdToLhs id) iEqualA e]
    Just (InitArray arr) -> [iAssign (varDeclIdToLhs id) iEqualA (iArrayCreateInit t (varDeclArrayDim id) arr)]

declToInit :: (Monad m) => RewriteM m JavaTerm [BlockStmtL]
declToInit (Common.extractF -> (project -> Just (LocalVars _ t decls)) : stmts) = return $ Common.insertF (assnStmts ++ stmts)
  where
    assns = concatMap (declToAssn t) (Common.extractF decls)

    assnStmts :: [JavaTerm BlockStmtL]
    assnStmts = map (iBlockStmt . iExpStmt) assns
declToInit x = return x

forDeclToInit :: (Monad m) => RewriteM m JavaTerm ForInitL
forDeclToInit = undefined


reifyDeclSpec :: DeclSpec -> JavaTerm BlockStmtL
reifyDeclSpec (DeclSpec mods typ id) = iLocalVars mods typ (Common.insertF [iVarDecl id (Common.insertF Nothing)])

-- Note: This misses constructor bodies
prependDecls :: (Monad m) => [DeclSpec] -> RewriteM m JavaTerm BlockL
prependDecls decls (project -> (Just (Block stmts))) = return $ iBlock $ Common.insertF $ (map reifyDeclSpec decls) ++ (Common.extractF stmts)

isBlock :: (Monad m) => GRewriteM (MaybeT m) JavaTerm
isBlock = promoteRF (idR :: RewriteM _ JavaTerm BlockL)

runOnOuterScopeT :: (Monad m, Monoid a) => GTranslateM m JavaTerm a -> GTranslateM m JavaTerm a
runOnOuterScopeT trans = mtryM . guardedT isBlock (const $ return mempty) (addFail $ (trans +>> foldT (runOnOuterScopeT trans)))

runOnOuterScopeR :: (Monad m) => GRewriteM m JavaTerm -> GRewriteM m JavaTerm
runOnOuterScopeR r = tryR $ guardedT isBlock idR (addFail (r >=> allR (runOnOuterScopeR r)))

declsToFront' :: (Monad m) => RewriteM m JavaTerm BlockL
declsToFront' block@(project -> Just (Block stmts)) = do
  decls <- execWriterT $ runOnOuterScopeT collectDecls stmts
  prependDecls decls <=< liftM iBlock $ runOnOuterScopeR (promoteR $ addFail declToInit) stmts

declsToFront :: (Monad m) => GRewriteM m JavaTerm
declsToFront = alltdR $ promoteR $ addFail declsToFront'

printJava :: JavaTerm CompilationUnitL -> IO ()
printJava = putStrLn . prettyPrint . untranslate

data TransformResult = TransformResult {transform :: String, result :: String}

instance ToJSON TransformResult where
  toJSON TransformResult{..} = object [
    "transform" .= transform,
    "result"    .= result  ]

runT = prettyPrint . untranslate . Common.untranslate . Common.translate . runIdentity

main = do
  [name] <- getArgs
  x <- parseFile name
  case x of
    Nothing -> putStrLn "Parse failed"
    Just t  -> do 
      putStrLn $ toString $ encode $
        [ TransformResult "removeWhiles" (runT $ removeWhiles t)
        , TransformResult "declsToFront" (runT $ declsToFront t)
        , TransformResult "vandilize" (runT $ vandalize t) ]
    
