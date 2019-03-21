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

type CanYTrans f = (HFunctor f, ShowHF f, Typeable f)
type MonadYogoJ m = (MonadYogo YJavaSig m)

class (CanYTrans f) => YTrans f t where
  ytrans :: (MonadYogoJ m) => YTransM m f J.MJavaSig YJavaSig t

ytranslate :: (MonadYogoJ m, YTrans J.MJavaSig t) => YTransTermM m J.MJavaSig YJavaSig t
ytranslate f = (ytrans . unTerm) f

-- ----------
-- Generic
-- ----------

instance {-# OVERLAPPABLE #-} (CanYTrans f) => YTrans f t where
  ytrans t = do
    traceM $ show $ typeRep (Proxy :: Proxy f)
    mzero
  -- ytrans = error $ show (typeRep (Proxy :: Proxy f))

instance {-# OVERLAPPING #-} (YTrans f1 t, YTrans f2 t) => YTrans (f1 :+: f2) t where
  ytrans = caseH' ytrans ytrans

instance (YTrans J.MJavaSig t) => YTrans Cx.ListF [t] where
  ytrans = ytransList ytranslate ytranslate

instance YTrans Cx.Ident AddressT where
  ytrans (Cx.Ident name :&: l) = insertNode [l] (IdentF name)

instance YTrans Cx.Assign MemValT where
  ytrans = ytransAssign ytranslate ytranslate

instance YTrans Cx.FunctionCall MemValT where
  ytrans = ytransFCall ytranslate ytranslate ytranslate

instance YTrans Cx.PositionalArgument ValueT where
  ytrans (Cx.PositionalArgument t :&: _) = ytranslate t

-- Could be address or value, depending on whether the function has a receiver argument
instance YTrans Cx.FunctionIdent ValueT where
  ytrans (Cx.FunctionIdent t :&: _) = ytranslate t

instance YTrans Cx.FunctionIdent AddressT where
  ytrans (Cx.FunctionIdent t :&: _) = ytranslate t

instance YTrans Cx.FunctionDef StatementT where
  ytrans (Cx.FunctionDef _ fname _ fbody :&: l) = ytranslateScope ytranslate ytranslate [l] fname fbody

instance YTrans Cx.Block [StatementT] where
  ytrans (Cx.Block t _ :&: l) = ytranslate t

-- ----------
-- Java specific
-- ----------


instance YTrans J.ClassDecl StatementT where
  ytrans (J.ClassDecl _ ident _ _ _ body :&: l) = do
    let ident' = case project' ident of
                 Just (J.IdentIsIdent ident') -> ident'
    ytranslateScope ytranslate ytranslate [l] ident' body

instance YTrans J.ClassBody [StatementT] where
  ytrans (J.ClassBody t :&: _) = ytranslate t

instance YTrans J.Decl StatementT where
  ytrans (J.MemberDecl t :&: _) = ytranslate t
  ytrans _ = error "J.Decl not implemented"

instance YTrans J.FunctionDefIsMemberDecl StatementT where
  ytrans (J.FunctionDefIsMemberDecl t :&: _) = ytranslate t

instance YTrans J.BlockIsFunctionBody [StatementT] where
  ytrans (J.BlockIsFunctionBody t :&: _) = ytranslate t

instance YTrans J.BlockStmt StatementT where
  ytrans (J.BlockStmt t :&: _) = ytranslate t
  ytrans _ = error "J.BlockStmt variant not implemented"

instance YTrans J.BlockStmtIsBlockItem StatementT where
  ytrans (J.BlockStmtIsBlockItem t :&: _) = traceM "ye" >> ytranslate t

instance YTrans J.Stmt StatementT where
  ytrans (J.ExpStmt t :&: _) = do
    _ :: JID ValueT <- ytranslate t
    return Statement
  ytrans _ = error "J.Stmt variant not implemented"

ytranslateLit :: (MonadYogoJ m) => [Label] -> J.MJavaTermLab J.LiteralL -> m (JID ValueT)
ytranslateLit labels (project' -> Just (J.Int n)) = insertNode labels $ ConstF $ IntegerF n

instance YTrans J.Exp ValueT where
  ytrans (J.Lit lit :&: l) = ytranslateLit [l] lit
  ytrans (J.ExpName name :&: l) = Q <$> ytranslate name <*> getMem >>= insertNode [l]

instance YTrans J.ExpIsRhs ValueT where
  ytrans (J.ExpIsRhs t :&: _) = ytranslate t

instance YTrans J.AssignIsExp ValueT where
  ytrans (J.AssignIsExp t :&: l) = ytranslate t >>= (insertNode [l]) . ValF

instance YTrans J.LhsIsLhs AddressT where
  ytrans (J.LhsIsLhs t :&: _) = ytranslate t

instance YTrans J.Name AddressT where
  ytrans (J.Name idents :&: l) = case project' idents of
                                   Just (Cx.ConsF ident _) -> ytranslate ident

instance YTrans J.Lhs AddressT where
  ytrans (J.NameLhs name :&: _) = ytranslate name

instance YTrans J.IdentIsIdent AddressT where
  ytrans (J.IdentIsIdent t :&: _) = ytranslate t

instance YTrans J.TypeDecl StatementT where
  ytrans (J.ClassTypeDecl t :&: l) = ytranslate t
  ytrans _ = error "J.TypeDecl variant not implemented"

instance YTrans J.MultiLocalVarDecl StatementT where
  ytrans (J.MultiLocalVarDecl _ decls :&: _) = do
    traceM "yo"
    _ :: JID [StatementT] <- ytranslate decls
    return Statement

instance YTrans Cx.SingleLocalVarDecl StatementT where
  ytrans = ytransSingleLocalVarDecl ytranslate ytranslate

instance YTrans Cx.IdentIsVarDeclBinder AddressT where
  ytrans (Cx.IdentIsVarDeclBinder t :&: _) = ytranslate t

instance YTrans J.VarInit ValueT where
  ytrans (J.InitExp t :&: _) = ytranslate t

instance YTrans J.VarInitIsLocalVarInit ValueT where
  ytrans (J.VarInitIsLocalVarInit t :&: _) = ytranslate t

instance YTrans J.MultiLocalVarDeclIsBlockStmt StatementT where
  ytrans (J.MultiLocalVarDeclIsBlockStmt t :&: _) = ytranslate t

instance YTrans J.CompilationUnit StatementT where
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
-- toGraphJava = undefined
