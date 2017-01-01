{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import Control.Monad.IO.Class

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Base64.URL as URL
import qualified Data.ByteString.Base64.Lazy as L64

import System.Process
import System.IO.Temp
import Web.Scotty
import Codec.Picture

import Process

sendImg :: String -> ActionM ()
sendImg inp = do
    bs <- liftIO $ withTempDirectory "." "pics" (runLatex inp)
    setHeader "Access-Control-Allow-Origin" "*"
    liftIO $ print (L64.encode bs)
    raw $ L64.encode bs

runLatex :: String -> FilePath -> IO BSL.ByteString
runLatex inp s = do
    liftIO $ writeFile (s ++ "/out.tex") (calc2latex inp)
    liftIO $ readProcess "./bodge.sh" [s] ""
    getPicture (s ++ "/out.png")

getPicture path = do
    img <- readImage path
    case img of
        Left err -> error err
        Right im -> case encodeDynamicPng im of
            Left err -> error err
            Right bs -> return bs

main = scotty 3000 $ do
    get "/:code" $ do
        code <- param "code"
        let inp = BS.unpack $ URL.decodeLenient code
        -- let lol = "\\x . x"
        sendImg inp
