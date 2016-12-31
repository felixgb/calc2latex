module Latexify where

import Data.List

import Syntax

class ToLatex a where
    latexify :: a -> String

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
