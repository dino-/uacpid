-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad.Error
import Data.Map hiding ( null )
import Data.Maybe
import Prelude hiding ( lookup )
import Network.Socket
import System.Environment
import System.Exit
import System.IO
import System.Log
import System.Posix.Files
import System.Posix.Signals hiding ( Handler )

import Uacpid.Conf
import Uacpid.Control.Monad.Error
import Uacpid.Events
import Uacpid.Log ( initLogging, logM )


data RunLevel = HALT | RUN | RESTART
   deriving Eq


throwSocketFileError :: (MonadError String m) => String -> m a
throwSocketFileError msgPrefix = throwError $ msgPrefix ++ " Make sure acpid is installed, is running, and that this path is correct. This config setting is in ~/.uacpid/uacpid.conf under the key acpidSocket"


openAcpidSocket :: (MonadError String m, MonadIO m) =>
   ConfMap -> m Handle
openAcpidSocket conf = do
   liftIO $ logM NOTICE "Establishing connection to acpid's socket..."

   acpidSocketPath <- lookupEString "acpidSocket" conf

   pathExists <- liftIO $ fileExist acpidSocketPath
   unless pathExists $ throwSocketFileError $
      "File " ++ acpidSocketPath ++ " does not exist."

   pathIsASocket <- liftIO $ liftM isSocket $ getFileStatus acpidSocketPath
   unless pathIsASocket $ throwSocketFileError $
      "File " ++ acpidSocketPath ++ " is not a socket."

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


{- When RunLevel is RESTART, this function sets up the daemon by 
   loading event handlers and establishing the connection to acpid, 
   sets the RunLevel to RUN and then starts the listenAcpi loop.
   Any other RunLevel makes it exit.
-}
connectLoop :: ConfMap -> MVar RunLevel -> IO ()
connectLoop conf mvRunStatus = do
   logM DEBUG "connectLoop called"

   runStatus <- readMVar mvRunStatus

   when (runStatus == RESTART) $ do
      handlers <- loadHandlers

      eHdl <- runErrorT $ openAcpidSocket conf
      either exitFail
         (\hdl -> do
            takeMVar mvRunStatus
            putMVar mvRunStatus RUN
            listenAcpi handlers mvRunStatus hdl
         )
         eHdl
      connectLoop conf mvRunStatus


{- Read lines from the socket and do something with them
   This function keeps calling itself as long as we're in RunLevel RUN
   Otherwise it falls back to connectLoop who takes the appropriate 
   action
-}
listenAcpi :: [Handler] -> MVar RunLevel -> Handle -> IO ()
listenAcpi handlers mvRunStatus hdl = do
   logM DEBUG "listenAcpi called"

   runStatus <- readMVar mvRunStatus

   when (runStatus == RUN) $ do
      -- No blocking unless the socket is ready with a line for us
      ready <- hReady hdl
      when ready $ do
         line <- hGetLine hdl
         logM INFO $ "Received from acpid: " ++ line
         executeHandlers line handlers

      -- Wait a bit, try again
      threadDelay 250000
      listenAcpi handlers mvRunStatus hdl


{- For a serious error that prevents further execution
-}
exitFail :: String -> IO ()
exitFail errMsg = do
   logM ERROR errMsg
   exitWith $ ExitFailure 1


{- Handler functions to adjust the RunLevel state based on signals 
   the daemon may receive
-}

handleExitSignals :: MVar RunLevel -> IO ()
handleExitSignals mvRunStatus = do
   takeMVar mvRunStatus

   logM NOTICE "uacpid daemon stopped"

   putMVar mvRunStatus HALT


handleHupSignal :: MVar RunLevel -> IO ()
handleHupSignal mvRunStatus = do
   takeMVar mvRunStatus

   logM NOTICE "sigHUP received"

   putMVar mvRunStatus RESTART


main :: IO ()
main = do
   -- Any args at all, give the user help and exit
   getArgs >>= \as -> unless (null as) usageAndExit

   conf <- getConf
   initLogging conf

   mvRunStatus <- newMVar RESTART

   -- Install signal handlers
   mapM_ (\signal -> installHandler signal 
      (Catch $ handleExitSignals mvRunStatus) Nothing) [sigINT, sigTERM]
   installHandler sigHUP (Catch $ handleHupSignal mvRunStatus) Nothing

   logM NOTICE "uacpid daemon started"
   logM NOTICE $ "Logging level " ++
      (fromJust $ lookup "logPriority" conf)

   -- The looping for events starts here
   connectLoop conf mvRunStatus

   -- If/when it makes it back here, exit gracefully
   exitWith ExitSuccess


usageAndExit :: IO ()
usageAndExit = do
   putStrLn $ unlines
      [ "uacpid - Userspace Advanced Configuration and Power Interface event daemon"
      , ""
      , "Please see man uacpid for detailed info"
      , ""
      , "Version 0.0.4  2009-Jul-19  Dino Morelli <dino@ui3.info>"
      ]

   exitWith ExitSuccess
