module Process where

import Control.Monad.Except

import Data.List

import Syntax
import Parser
import Infer
import Latexify

process :: String -> String
process inp = case runExcept (parseExpr inp >>= inferExpr) of
    Left err -> docErr err
    Right (ty, tex) -> docString tex

docErr :: LangErr -> String
docErr err =
    intercalate "\n" [ "\\documentclass["
                     , "preview,"
                     , "border={20px 20px 20px 20px}"
                     , "]{standalone}"
                     , "\\begin{document}"
                     , latexify err
                     , "\\end{document}"
                     ]

docString :: [Texlevel] -> String
docString tex = 
    intercalate "\n" [ "\\documentclass["
                     , "preview,"
                     , "border={20px 20px 20px 20px}"
                     , "]{standalone}"
                     , "\\usepackage{ebproof}"
                     , "\\usepackage[showframe]{geometry}"
                     , "\\geometry{"
                     , "paperwidth=1000px,"
                     , "paperheight=1000px,"
                     , "margin=50px"
                     , "}"
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

