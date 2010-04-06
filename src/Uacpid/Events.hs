-- Copyright: 2009, 2010 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

module Uacpid.Events
   ( Handler (..), loadHandlers, executeHandlers )
   where

import Control.Monad.Error
import Data.List
import Data.Maybe
import System.Directory
import System.FilePath
import System.Log
import System.Process
import Text.Regex ( matchRegex, mkRegex )

import Uacpid.Conf ( ConfMap, getConfDir, parseToMap )
import Uacpid.Control.Monad.Error
import Uacpid.Log ( logM )


data Handler = Handler
   { evName :: String
   , evEventRe :: String
   , evAction :: String
   }


lookupHandlerE :: (MonadError String m) =>
   String -> String -> ConfMap -> m String
lookupHandlerE name = lookupEWith
   (\j -> "Handler " ++ name ++ ": key " ++ j ++ " not found")


loadHandler :: (String, FilePath) -> IO (Maybe Handler)
loadHandler (name, path) = do
   -- Parse the event file into a map
   evMap <- liftM parseToMap $ readFile path

   -- Extract data from it to make an Handler, with error reporting
   eitherStringHandler <- runErrorT $ do
      event <- lookupHandlerE name "event" evMap
      action <- lookupHandlerE name "action" evMap
      return $ Handler name event action

   result <- case eitherStringHandler of
      Left emsg -> do
         logM WARNING emsg
         return Nothing
      Right ev -> return $ Just ev

   return result


loadHandlers :: IO [Handler]
loadHandlers = do
   confDir <- getConfDir
   let eventsDir = confDir </> "events"

   {- Load the event handler files. Files starting with . and ending 
      with ~ are discarded. We don't want . and .. of course, but you 
      can also disable one by make it look like events/.foo or 
      events/foo~
   -}
   eventFiles <- liftM 
      ( filter (\file -> not $ any ($ file)
         [ isPrefixOf ".", isSuffixOf "~" ]))
      $ getDirectoryContents eventsDir

   -- Construct a list of (filename, /path/to/filename)
   let eventPairs = map (\n -> (n, eventsDir </> n)) eventFiles

   -- Load these files and parse them
   events <- liftM catMaybes $ mapM loadHandler eventPairs

   let names = map evName events

   -- Log what we loaded for informational purposes
   if (null names)
      then logM NOTICE "No valid event handlers were found"
      else logM NOTICE $ "Handlers loaded: " ++
         (intercalate " " names)

   return events


executeHandlers :: String -> [Handler] -> IO ()
executeHandlers acpiHandler es = do
   let responders = filter (\e -> isJust $
         matchRegex (mkRegex (evEventRe e)) acpiHandler) es

   mapM_ (runCommand . evAction) responders
