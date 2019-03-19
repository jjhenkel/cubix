{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Java.Trans (
  YJavaSig
  , toGraphJava
  ) where

import Data.Proxy ( Proxy(..) )
import Debug.Trace
import Data.Typeable

import Control.Lens ( makeLenses, over, use, (^.), (.=), (%=), (+=), (-=) )
import Control.Monad ( MonadPlus, mzero, liftM )
import Control.Monad.Trans.Maybe ( runMaybeT )
import Control.Monad.Identity ( Identity(..), runIdentity )
import Control.Monad.State ( MonadState, execStateT, evalStateT, runStateT, modify, get )
import Data.Comp.Multi
import Data.Comp.Multi.Strategic
import Data.Map ( Map )
import qualified Data.Map as Map
import Data.Maybe ( fromJust )

import Cubix.Language.Info
import qualified Cubix.Language.Parametric.Syntax as Cx
import qualified Cubix.Language.Java.Parametric.Common as J

import Common.Trans

type YJavaSig = YGenericSig
type JID t = ID YJavaSig t

type MonadYogoJ m = (MonadYogo YJavaSig m)
type CanYTransJ f = (CanYTrans f)

instance YTrans J.ClassDecl J.MJavaSig YJavaSig StatementT where
  ytrans (J.ClassDecl _ ident _ _ _ body :&: l) = do
    let ident' = case project' ident of
                 Just (J.IdentIsIdent ident') -> ident'
    ytransScope [l] ident' body

instance YTrans J.ClassBody J.MJavaSig YJavaSig [StatementT] where
  ytrans (J.ClassBody t :&: _) = ytranslate t

instance YTrans J.Decl J.MJavaSig YJavaSig StatementT where
  ytrans (J.MemberDecl t :&: _) = ytranslate t
  ytrans _ = error "J.Decl not implemented"

instance YTrans J.FunctionDefIsMemberDecl J.MJavaSig YJavaSig StatementT where
  ytrans (J.FunctionDefIsMemberDecl t :&: _) = ytranslate t

instance YTrans J.BlockStmt J.MJavaSig YJavaSig StatementT where
  ytrans (J.BlockStmt t :&: _) = ytranslate t
  ytrans _ = error "J.BlockStmt variant not implemented"

instance YTrans J.BlockStmtIsBlockItem J.MJavaSig YJavaSig StatementT where
  ytrans (J.BlockStmtIsBlockItem t :&: _) = ytranslate t

instance YTrans J.Stmt J.MJavaSig YJavaSig StatementT where
  ytrans (J.ExpStmt t :&: _) = do
    _ :: JID ValueT <- ytranslate t
    return Statement
  ytrans _ = error "J.Stmt variant not implemented"

instance YTrans J.AssignIsExp J.MJavaSig YJavaSig ValueT where
  ytrans (J.AssignIsExp t :&: l) = ytranslate t >>= (insertNode [l]) . ValF

instance YTrans J.LhsIsLhs J.MJavaSig YJavaSig AddressT where
  ytrans (J.LhsIsLhs t :&: _) = ytranslate t

instance YTrans J.Lhs J.MJavaSig YJavaSig AddressT where
  ytrans (J.NameLhs name :&: l) = do
    let ident = case project' name of
                   Just (J.Name x) ->
                     case project' x of
                       Just (Cx.ConsF ident _) -> ident
    ytranslate ident

instance YTrans J.IdentIsIdent J.MJavaSig YJavaSig ValueT where
  ytrans (J.IdentIsIdent t :&: _) = ytranslate t

instance YTrans J.TypeDecl J.MJavaSig YJavaSig StatementT where
  ytrans (J.ClassTypeDecl t :&: l) = ytranslate t
  ytrans _ = error "J.TypeDecl variant not implemented"

instance YTrans J.CompilationUnit J.MJavaSig YJavaSig StatementT where
  ytrans (J.CompilationUnit _ _ decl :&: _) = do
    newScope $ Name "CompilationUnit"
    _ :: JID [StatementT] <- ytranslate decl
    return Statement

ytransCompilationUnit :: (MonadYogoJ m)
                      => TranslateM m J.MJavaTermLab J.CompilationUnitL (JID StatementT)
ytransCompilationUnit = ytranslate

initState :: YogoState YJavaSig
initState = YogoState [] [] Map.empty 0 0

fileToGraph :: J.MJavaTermLab l -> YFile YJavaSig
fileToGraph t =
  let (a, state) = fromJust $ runIdentity $ runMaybeT $ runStateT ((onetdT $ promoteTF ytransCompilationUnit) t) initState in
    state ^. file

toGraphJava :: Project J.MJavaSig -> YProject YJavaSig
toGraphJava = Map.map (\(E t) -> fileToGraph t)
