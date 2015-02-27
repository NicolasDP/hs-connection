-- |
-- Module      : Client
-- License     : BSD-style
-- Maintainer  : Nicolas DI PRIMA <nicolas@di-prima.fr>
-- Stability   : experimental
-- Portability : portable
--

module Main (main) where

import Control.Monad
import qualified Data.ByteString.Char8 as BC
import Data.Char
import Network.Connection

import System.Environment
import System.Exit

sendLineTo :: Int -> IO ()
sendLineTo portnumber = do
    ctx <- initConnectionContext
    con <- connectTo ctx $ ConnectionParams
                { connectionHostname  = "localhost"
                , connectionPort      = fromIntegral portnumber
                , connectionUseSecure = Nothing
                , connectionUseSocks  = Nothing
                }

    forever $ do
        str <- getLine
        connectionPut con $ BC.pack (str ++ "\n")
        case map toLower str of
            "starttls" -> connectionSetSecure ctx con (TLSSettingsSimple True False False)
            "close"    -> do
                connectionClose con
                exitSuccess
            _          -> return ()

main :: IO ()
main = do
    args <- getArgs
    case args of
        []     -> error "expecting a port number to listen on"
        [port] -> sendLineTo (read port)
        _      -> error "usage: server <port-number>"
