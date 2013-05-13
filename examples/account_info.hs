{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}

module Main where

import Control.Monad
import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Char as B

import Data.Char
import Data.Maybe

import Network.Socket

import System.Console.CmdArgs.Implicit
import System.IO
import System.Log.FastLogger
import qualified System.IO.Streams as S
import qualified System.IO.Streams.Network as S


ibHandshake :: Socket -> IO ()
ibHandshake connsock = do
    (net_is, net_os) <- S.socketToStreams connsock
    S.write (Just (BC.snoc "59" (chr 0))) net_os
    server_version <- S.read net_is 
    B.putStr $ fromJust server_version
    return ()


ibConnect :: HostName -> ServiceName -> IO ()
ibConnect hostname port = withSocketsDo $ do
    addrinfos <- getAddrInfo Nothing (Just hostname) (Just port)
    let serveraddr = head addrinfos
    sock <- socket (addrFamily serveraddr) Stream defaultProtocol
    setSocketOption sock KeepAlive 1
    connect sock (addrAddress serveraddr)
    
    connected <- isConnected sock
    if (not connected) 
        then putStrLn "connect failed."
        else ibHandshake sock



data Opts = Opts
    { verbose :: Bool
    , host :: String
    , port :: String
    } deriving (Data, Typeable, Show, Eq)


progOpts :: Opts
progOpts = Opts
    { host = def &= help "server host"
    , port = def &= help "server port"
    , verbose = def &= help "verbose mode"
    }


getOpts :: IO Opts
getOpts = cmdArgs $ progOpts
    &= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
    &= program _PROGRAM_NAME
    &= help _PROGRAM_DESC
    &= helpArg [explicit, name "help", name "h"]
    &= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]


_PROGRAM_NAME :: String
_PROGRAM_NAME = "account_info"

_PROGRAM_VERSION :: String
_PROGRAM_VERSION = "0.0.1"

_PROGRAM_INFO :: String
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION

_PROGRAM_DESC :: String
_PROGRAM_DESC = "account info"

_COPYRIGHT :: String
_COPYRIGHT = "BSD-3"


main :: IO ()
main = do 
    opts <- getOpts

    let server_host = host $ opts
        server_port = port $ opts
    logger <- mkLogger True stdout
    loggerPutStr logger $ map toLogStr ["connecting ", server_host, ":", server_port, "\n"]
    c <- async (ibConnect server_host server_port)
    wait c
    return ()
