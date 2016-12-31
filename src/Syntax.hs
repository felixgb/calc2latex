module Syntax where

import Control.Monad.Except

type ThrowsErr = Except String

type Var = String

data Expr
    = Var Var
    | App Expr Expr
    | Lam Var Expr
    deriving (Show, Eq, Ord)

data Type
    = TyVar Var
    | TyArr Type Type
    deriving (Eq, Ord)

data Texlevel
    = TexVar String Type
    | TexAbs String Expr Type Type
    | TexApp Expr Expr Type
    deriving (Eq, Ord)

instance Show Type where
    show (TyVar x) = x
    show (TyArr t1 t2) = concat ["(", show t1, " -> ", show t2, ")"]
