-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Uacpid.Events
   ( Event (..), loadEvents )
   where

import Control.Monad
import Control.Monad.Error
import Data.List
import Data.Maybe
import System.Directory
import System.FilePath
import System.Log

import Uacpid.Conf ( getConfDir, parseToMap )
import Uacpid.Control.Monad.Error
import Uacpid.Log ( logM )


data Event = Event
   { evName :: String
   , evEventRe :: String
   , evAction :: String
   }
   deriving Show  -- FIXME


lookupEventE name = lookupEWith
   (\j -> "Event handler " ++ name ++ ": key " ++ j ++ " not found")


loadEvent :: (String, FilePath) -> IO (Maybe Event)
loadEvent (name, path) = do
   -- Parse the event file into a map
   evMap <- liftM parseToMap $ readFile path

   -- Extract data from it to make an Event, with error reporting
   eitherStringEvent <- runErrorT $ do
      event <- lookupEventE name "event" evMap
      action <- lookupEventE name "action" evMap
      return $ Event name event action

   result <- case eitherStringEvent of
      Left emsg -> do
         logM WARNING emsg
         return Nothing
      Right ev -> return $ Just ev

   return result


loadEvents :: IO [Event]
loadEvents = do
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
   events <- liftM catMaybes $ mapM loadEvent eventPairs

   let names = map evName events

   -- Log what we loaded for informational purposes
   if (null names)
      then logM NOTICE "No valid event handlers were found"
      else logM NOTICE $ "Event handlers loaded: " ++
         (intercalate " " names)

   return events
