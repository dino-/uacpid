-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad.Error
import Data.Map
import Data.Maybe
import Prelude hiding ( lookup )
import Network.Socket
import System.Directory
import System.Exit
import System.IO
import System.Log
import System.Posix.Signals

import Uacpid.Conf
import Uacpid.Log ( initLogging, logM )


-- Lookup a key in a map in Error monad
lookupE :: (MonadError String m) =>
   String -> Map String a -> m a
lookupE k m = maybe
   (throwError ("Required key " ++ k ++ " not found in config"))
   return $ lookup k m


openAcpidSocket :: (MonadError String m, MonadIO m) =>
   ConfMap -> m Handle
openAcpidSocket conf = do
   liftIO $ logM NOTICE "Establishing connection to acpid's socket..."

   acpidSocketPath <- lookupE "acpidSocket" conf

   exists <- liftIO $ doesFileExist acpidSocketPath
   unless exists $
      throwError $ "Socket " ++ acpidSocketPath ++ " does not exist. Make sure acpid is installed, is running, and that this path is correct. This config setting is in ~/.uacpid/uacpid.conf under the key acpidSocket"

   hdl <- liftIO $ do
      -- Open the UNIX domain socket
      s <- socket AF_UNIX Stream defaultProtocol
      connect s $ SockAddrUnix acpidSocketPath

      -- Turn it into an ordinary handle
      h <- socketToHandle s ReadMode
      hSetBuffering h LineBuffering

      return h

   liftIO $ logM NOTICE "Connected."

   return hdl


-- Read lines from the socket and do something with them
listenAcpi :: MVar Bool -> Handle -> IO ()
listenAcpi mvRunStatus hdl = do
   stopNow <- readMVar mvRunStatus

   unless stopNow $ do
      -- No blocking unless the socket is ready with a line for us
      ready <- hReady hdl
      when ready $ do
         line <- hGetLine hdl
         logM INFO $ "Received from acpid: " ++ line

      -- Wait a bit, try again
      threadDelay 250000
      listenAcpi mvRunStatus hdl


exitFail :: String -> IO ()
exitFail errMsg = do
   logM ERROR errMsg
   exitWith $ ExitFailure 1


exitHandler :: MVar Bool -> IO ()
exitHandler mvRunStatus = do
   -- We don't care what it is, we just want to take it from everyone else
   takeMVar mvRunStatus

   logM NOTICE "uacpid daemon stopped"

   -- Make note in the state to stop the other thread
   putMVar mvRunStatus True


main :: IO ()
main = do
   conf <- getConf
   initLogging conf

   mvRunStatus <- newMVar False

   -- Install signal handling
   mapM_ (\signal -> installHandler signal 
      (Catch $ exitHandler mvRunStatus) Nothing) [sigINT, sigTERM]

   logM NOTICE "uacpid daemon started"
   logM NOTICE $ "Logging level " ++
      (fromJust $ lookup "logPriority" conf)

   eHdl <- runErrorT $ openAcpidSocket conf
   either exitFail (listenAcpi mvRunStatus) eHdl

   exitWith $ ExitSuccess
