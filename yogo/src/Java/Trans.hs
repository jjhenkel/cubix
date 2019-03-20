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
  , JBlank(..)
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

data JBlank (e :: * -> *) l where
  JBlank :: JBlank e l

type YJavaSig = JBlank :+: YGenericSig
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

instance YTrans J.BlockIsFunctionBody J.MJavaSig YJavaSig [StatementT] where
  ytrans (J.BlockIsFunctionBody t :&: _) = ytranslate t

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

ytransLit :: (MonadYogoJ m) => [Label] -> J.MJavaTermLab J.LiteralL -> m (JID ValueT)
ytransLit labels (project' -> Just (J.Int n)) = insertNode labels $ ConstF $ IntegerF n

instance YTrans J.Exp J.MJavaSig YJavaSig ValueT where
  ytrans (J.Lit lit :&: l) = ytransLit [l] lit
  ytrans (J.ExpName name :&: l) = Q <$> ytranslate name <*> getMem >>= insertNode [l]

instance YTrans J.ExpIsRhs J.MJavaSig YJavaSig ValueT where
  ytrans (J.ExpIsRhs t :&: _) = ytranslate t

instance YTrans J.AssignIsExp J.MJavaSig YJavaSig ValueT where
  ytrans (J.AssignIsExp t :&: l) = ytranslate t >>= (insertNode [l]) . ValF

instance YTrans J.LhsIsLhs J.MJavaSig YJavaSig AddressT where
  ytrans (J.LhsIsLhs t :&: _) = ytranslate t

instance YTrans J.Name J.MJavaSig YJavaSig AddressT where
  ytrans (J.Name idents :&: l) = case project' idents of
                                   Just (Cx.ConsF ident _) -> ytranslate ident

instance YTrans J.Lhs J.MJavaSig YJavaSig AddressT where
  ytrans (J.NameLhs name :&: _) = ytranslate name

instance YTrans J.IdentIsIdent J.MJavaSig YJavaSig AddressT where
  ytrans (J.IdentIsIdent t :&: _) = ytranslate t

instance YTrans J.TypeDecl J.MJavaSig YJavaSig StatementT where
  ytrans (J.ClassTypeDecl t :&: l) = ytranslate t
  ytrans _ = error "J.TypeDecl variant not implemented"

instance YTrans J.VarInit J.MJavaSig YJavaSig ValueT where
  ytrans (J.InitExp t :&: _) = ytranslate t

instance YTrans J.VarInitIsLocalVarInit J.MJavaSig YJavaSig ValueT where
  ytrans (J.VarInitIsLocalVarInit t :&: _) = ytranslate t

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
