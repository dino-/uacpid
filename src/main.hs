-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.Error
import Data.Map
import Data.Maybe
import Fez.Data.Conf
import Prelude hiding ( lookup )
import Network.Socket
import System.Directory
import System.Exit
import System.IO
import System.Log

import Uacpid.Conf
import Uacpid.Log ( initLogging, logM )


-- Lookup a key in a map in Error monad
lookupE :: (MonadError String m) =>
   String -> Map String a -> m a
lookupE k m = maybe
   (throwError ("required key " ++ k ++ " not found in config"))
   return $ lookup k m


openAcpidSocket :: (MonadError String m, MonadIO m) =>
   ConfMap -> m Handle
openAcpidSocket conf = do
   acpidSocketPath <- lookupE "acpidSocket" conf

   exists <- liftIO $ doesFileExist acpidSocketPath
   unless exists $
      throwError $ "socket not present at " ++ acpidSocketPath

   hdl <- liftIO $ do
      -- Open the UNIX domain socket
      s <- socket AF_UNIX Stream defaultProtocol
      connect s $ SockAddrUnix acpidSocketPath

      -- Turn it into an ordinary handle
      h <- socketToHandle s ReadMode
      hSetBuffering h LineBuffering

      return h

   return hdl


-- Read lines from the socket and do something with them
listenAcpi :: Handle -> IO ()
listenAcpi hdl = forever $ do
   line <- hGetLine hdl
   logM INFO $ "received from acpid: " ++ line


exitFail :: String -> IO ()
exitFail errMsg = do
   logM ERROR errMsg
   exitWith $ ExitFailure 1


main :: IO ()
main = do
   conf <- getConf
   initLogging conf

   logM NOTICE "uacpid daemon started"

   logM NOTICE "uacpid daemon started"
   logM NOTICE $ "logging level " ++
      (fromJust $ lookup "logPriority" conf)

   eHdl <- runErrorT $ openAcpidSocket conf
   either exitFail listenAcpi eHdl

   -- FIXME This never gets called when we ctrl-c
   logM NOTICE "uacpid daemon stopped"
