module Process where

import Control.Monad.Except

import Data.List

import Syntax
import Parser
import Infer
import Latexify

go :: String -> IO ()
go inp = do
    let (ty, tex) = process inp
    putStrLn $ docString tex

process :: String -> (Type, [Texlevel])
process inp = case runExcept (parseExpr inp >>= inferExpr) of
    Left err -> error err
    Right (ty, tex) -> (ty, tex)

docString :: [Texlevel] -> String
docString tex = 
    intercalate "\n" [ "\\documentclass["
                     , "preview,"
                     , "border={20px 20px 20px 20px}"
                     , "]{standalone}"
                     , "\\usepackage{ebproof}"
                     , "\\usepackage{amssymb} % To provide the \\varnothing symbol"
                     , "\\newcommand{\\nothing}{\\varnothing}"
                     , "\\newcommand{\\Var}[2]{\\Infer0[(Var)]{\\Gamma , #1 : #2 \\vdash #1 : #2}} % var varty"
                     , "\\newcommand{\\Abs}[4]{\\Infer1[(Abs)]{\\Gamma \\vdash \\lambda #1 . #2 : #3 \\to #4}} % var bod varty bodyty"
                     , "\\newcommand{\\App}[3]{\\Infer2[(App)]{\\Gamma \\vdash #1 #2 : #3}} % M N A B"
                     , "\\begin{document}"
                     , "\\begin{prooftree}"
                     , latexify tex
                     , "\\end{prooftree}"
                     , "\\end{document}"
                     ]

