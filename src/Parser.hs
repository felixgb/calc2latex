{-# LANGUAGE OverloadedStrings #-}

module Parser (
    parseExpr
) where

import Control.Monad.Except

import Text.Parsec
import Text.Parsec.String (Parser)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import Lexer
import Syntax

variable :: Parser Expr
variable = do
    x <- identifier
    return (Var x)

lambda :: Parser Expr
lambda = do
    reservedOp "\\"
    args <- many identifier
    reservedOp "."
    body <- expr
    return $ foldr Lam body args

aexp :: Parser Expr
aexp = parens expr
    <|> lambda
    <|> variable

term :: Parser Expr
term = Ex.buildExpressionParser table aexp

table = []

expr :: Parser Expr
expr = do
    es <- many1 term
    return (foldl1 App es)

parseExpr :: String -> ThrowsErr Expr
parseExpr input = case parse (contents expr) "<stdin>" input of
    (Right ok) -> return ok
    (Left err) -> throwError (ErrParse err)
