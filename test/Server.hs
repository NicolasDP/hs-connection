-- |
-- Module      : Server
-- License     : BSD-style
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : portable
--

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative
import Control.Monad
import Control.Concurrent
import Data.Char
import qualified Data.ByteString.Char8 as BC
import Network.Connection

import System.Environment

import qualified Network.TLS as TLS

listenOnPort :: Int -> IO ()
listenOnPort portnumber = do
    certificate <- either error id <$> TLS.credentialLoadX509 "cacert.pem" "privkey.pem"

    ctx <- initConnectionContext' $ TLS.Credentials [certificate]
    lis <- listenOn ctx port

    forever $ do
        conn <- accept Nothing lis
        putStrLn $ "connection established with: " ++ (show $ connectionID conn)
        forkIO $ do
            cmd <- BC.unpack <$> connectionGetLine 512 conn
            case map toLower cmd of
                "starttls" -> do
                    putStrLn "Set The connection secure"
                    connectionSetSecure ctx conn defaultTLSServerSettings
                "close"    -> connectionClose conn
                _          -> putStrLn $ "data received: " ++ show cmd
  where
    port :: PortID
    port = PortNumber $ fromIntegral portnumber

main :: IO ()
main = do
    args <- getArgs
    case args of
        []     -> error "expecting a port number to listen on"
        [port] -> listenOnPort $ read port
        _      -> error "usage: server <port-number>"
