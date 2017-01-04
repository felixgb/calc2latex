{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Infer where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Except

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Syntax

type Constraint = (Type, Type)

type Ctx = Map.Map String Type

type Infer a = StateT [String] (WriterT [Texlevel] ThrowsErr) a

--vars = cycle $ map (:[]) ['A'..'Z']
vars = concatMap (\n -> [c : show n | c <- ['A'..'Z']]) [1..]

freshVar :: Infer Type
freshVar = do
    name <- fmap (TyVar . head) get
    modify tail
    return name

newtype Subst = Subst (Map.Map String Type)
    deriving (Eq, Ord, Show, Monoid)

class Substitutable a where
    apply :: Subst -> a -> a

instance Substitutable Type where
    apply (Subst s) t@(TyVar name) = Map.findWithDefault t name s
    apply s (e1 `TyArr` e2) = apply s e1 `TyArr` apply s e2

instance Substitutable Constraint where
    apply s (t1, t2) = (apply s t1, apply s t2)

instance Substitutable a => Substitutable [a] where
    apply = map . apply

instance Substitutable Ctx where
    apply s ctx = Map.map (apply s) ctx

instance Substitutable Texlevel where
    apply s (TexVar x ty) = TexVar x (apply s ty)
    apply s (TexAbs x bod vty bty) = TexAbs x bod (apply s vty) (apply s bty)
    apply s (TexApp m n a) = TexApp m n (apply s a)

ctxLookup :: String -> Ctx -> Infer Type
ctxLookup name ctx = case Map.lookup name ctx of
    (Just ty) -> return ty
    Nothing -> throwError $ ErrUnboundVar name

recon :: Expr -> Ctx -> [Constraint] -> Infer (Type, [Constraint])
recon expr ctx cs = case expr of
    
    (Var name) -> do
        ty <- ctxLookup name ctx
        tell [TexVar name ty]
        return (ty, cs)

    (App func arg) -> do
        (funcTy, cs') <- recon func ctx cs
        (argTy, cs'') <- recon arg ctx cs'
        newvar <- freshVar
        -- tell [(funcTy, argTy `TyArr` newvar)]
        tell [TexApp func arg newvar]
        return (newvar, (funcTy, argTy `TyArr` newvar) : cs'')

    (Lam var body) -> do
        newvar <- freshVar
        let ctx' = Map.insert var newvar ctx
        (bodyTy, cs') <- recon body ctx' cs
        tell [TexAbs var body newvar bodyTy]
        return (newvar `TyArr` bodyTy, cs')

emptyCtx = Map.empty

runInfer :: Expr -> ThrowsErr ((Type, [Constraint]), [Texlevel])
runInfer expr = do
    let writer = evalStateT (recon expr emptyCtx []) vars
    runWriterT writer

type Unifier = (Subst, [Constraint])

emptySubst :: Subst
emptySubst = mempty

compose :: Subst -> Subst -> Subst
-- More efficent order of substitutions possible?
compose (Subst s1) (Subst s2) = Subst $ Map.map (apply (Subst s1)) s2 `Map.union` s1

inferExpr :: Expr -> ThrowsErr (Type, [Texlevel])
inferExpr inp = do
    ((ty, cs), tex) <- runInfer inp
    subst <- runSolve cs
    return (apply subst ty, apply subst tex)

runSolve :: [Constraint] -> ThrowsErr Subst
runSolve cs = solver (emptySubst, cs)

solver :: Unifier -> ThrowsErr Subst
solver (su, cs) = case cs of
    [] -> return su
    ((t1, t2): cs0) -> do
        su1 <- unifies t1 t2
        solver (su1 `compose` su, apply su1 cs0)

unifyMany :: [Type] -> [Type] -> ThrowsErr Subst
unifyMany [] [] = return emptySubst
unifyMany (t1 : ts1) (t2 : ts2) = do
    su1 <- unifies t1 t2
    su2 <- unifyMany (apply su1 ts1) (apply su1 ts2)
    return (su2 `compose` su1)
unifyMany t1 t2 = throwError $ ErrUnify t1 t2

unifies :: Type -> Type -> ThrowsErr Subst
unifies t1 t2
    | t1 == t2 = return emptySubst
unifies (TyVar x) t = x `bind` t
unifies t (TyVar x) = x `bind` t
unifies (TyArr t1 t2) (TyArr t3 t4) = unifyMany [t1, t2] [t3, t4]
unifies t1 t2 = throwError $ ErrUnify [t1] [t2]

bind :: String -> Type -> ThrowsErr Subst
bind a t
    | t == TyVar a = return emptySubst
    | occursCheck a t = throwError $ ErrOccursCheck a t
    | otherwise = return (Subst $ Map.singleton a t)

freeTyVar :: Type -> Set.Set String
freeTyVar (TyVar n) = Set.singleton n
freeTyVar (TyArr t1 t2) = freeTyVar t1 `Set.union` freeTyVar t2

occursCheck :: String -> Type -> Bool
occursCheck a t = a `Set.member` freeTyVar t
