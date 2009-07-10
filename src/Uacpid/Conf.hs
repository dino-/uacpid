-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Uacpid.Conf
   ( getConf
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


getConf :: IO ConfMap
getConf = do
   -- Construct some paths using $HOME
   homeDir <- getEnv "HOME"
   let confDir = homeDir </> ".uacpid"
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
   let realConf = insertWith (</>) "logDir" homeDir loadedConf

   -- First time log directory creation
   createDirectoryIfMissing True $ fromJust $ lookup "logDir" realConf

   return realConf
