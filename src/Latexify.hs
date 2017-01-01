module Latexify where

import Text.Parsec.Error

import Data.List

import Syntax

class ToLatex a where
    latexify :: a -> String

instance ToLatex LangErr where
    latexify (ErrParse err) = "Parse error!"
    latexify (ErrUnboundVar var) = "Unbound variable: " ++ var
    latexify (ErrUnify t1 t2) = concat ["Can't unify: $", latexify t1, "$ and $", latexify t2, "$"]
    latexify (ErrOccursCheck t1 t2) = 
        concat ["Occurs check: var $", t1, "$ occurs in: $", latexify t2, "$"]

instance ToLatex Expr where
    latexify (Var n) = n
    latexify (App m n) = latexify m ++ latexify n
    latexify (Lam x m) = concat ["(\\lambda ", x, " . ", latexify m, ")"]

instance ToLatex Type where
    latexify (TyVar n) = n
    latexify (TyArr t1 t2) = concat ["(", latexify t1, " \\to ", latexify t2, ")"]

instance ToLatex Texlevel where
    latexify (TexVar x ty) = concat ["\\Var{", x, "}{", latexify ty, "}"]
    latexify (TexAbs x bod vty bty) =
        concat [ "\\Abs{"
               , x
               , "}{"
               , latexify bod
               , "}{"
               , latexify vty
               , "}{"
               , latexify bty
               , "}"
               ]
    latexify (TexApp m n aty) =
        concat [ "\\App{"
               , latexify m
               , "}{"
               , latexify n
               , "}{"
               , latexify aty
               , "}"
               ]

instance ToLatex a => ToLatex [a] where
    latexify xs = intercalate "\n" (map latexify xs)
