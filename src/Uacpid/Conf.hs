-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Uacpid.Conf
   ( getConf
   )
   where

import Control.Monad
import Data.Map ( fromList, insertWith )
import Fez.Data.Conf
import System.Directory
import System.Environment
import System.FilePath
--import System.IO


{- NOTE: This should be kept in sync with the commented-out defaults
   in resources/default-uacpid.conf
   This file is copied to ~/.uacpid/uacpid.conf on first run and
   serves as documentation of default values used here.
-}
defaultConf :: ConfMap
defaultConf = fromList
   [ ( "logDir", "var/log" )
   , ( "logPriority", "WARNING" )
   ]


getConf :: IO ConfMap
getConf = do
   -- Construct some paths using $HOME
   homeDir <- getEnv "HOME"
   let confDir = homeDir </> ".uacpid"
   let confFilePath = confDir </> "uacpid.conf"

   createDirectoryIfMissing True confDir

   -- Load or create conf file $HOME/.uacpid/uacpid.conf
   confExists <- doesFileExist confFilePath
   origConf <- if confExists
      then liftM parseToMap $ readFile confFilePath
      else do
         copyFile
            -- FIXME This must be changed, very bad
            "/home/dino/dev/uacpid/trunk/resources/default-uacpid.conf"
            confFilePath
         return defaultConf

   -- Replace the log path with an absolute one using $HOME
   let realConf = insertWith (</>) "logDir" homeDir origConf

   return realConf
