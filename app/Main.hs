module Main where

import System.Environment

import Process

main :: IO ()
main = do
    src <- fmap head getArgs
    go src
