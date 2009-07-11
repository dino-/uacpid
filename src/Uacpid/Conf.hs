-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Uacpid.Conf
   ( getConf
   , getConfDir
   )
   where

import Control.Monad
import Data.Map ( insertWith, lookup )
import Data.Maybe
import Fez.Data.Conf
import Prelude hiding ( lookup )
import System.Directory
import System.Environment
import System.FilePath

import Paths_uacpid


getHomeDir :: IO String
getHomeDir = getEnv "HOME"


getConfDir :: IO String
getConfDir = liftM (flip (</>) ".uacpid") getHomeDir


getConf :: IO ConfMap
getConf = do
   -- Construct some paths using $HOME
   homeDir <- getHomeDir
   confDir <- getConfDir
   let confFilePath = confDir </> "uacpid.conf"

   -- First time conf directory creation
   createDirectoryIfMissing True confDir

   -- Create the conf for the first time if necessary
   confExists <- doesFileExist confFilePath
   unless confExists $ do
      defaultConfFilePath <- getDataFileName "default-uacpid.conf"
      copyFile defaultConfFilePath confFilePath

   -- Load the conf
   loadedConf <- liftM parseToMap $ readFile confFilePath

   -- Replace the log path with an absolute one using $HOME
   let realConf = insertWith (</>) "logPath" homeDir loadedConf

   -- While we're here doing first time things, make sure there's
   -- an events dir
   createDirectoryIfMissing True $ confDir </> "events"

   -- First time log directory creation
   let logDir = dropFileName $ fromJust $ lookup "logPath" realConf
   createDirectoryIfMissing True logDir

   return realConf
