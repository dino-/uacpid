-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Uacpid.Log
   ( initLogging, logM )
   where

import Control.Monad ( liftM )
import Data.Map ( lookup )
import Data.Maybe
import Data.Time.Clock ( getCurrentTime )
import Data.Time.Format ( formatTime )
import Data.Time.LocalTime ( utcToLocalZonedTime )
import Fez.Data.Conf
import Prelude hiding ( lookup )
import System.Locale ( defaultTimeLocale )
import System.Log.Handler.Simple ( fileHandler )
import qualified System.Log.Logger as L ( logM )
import           System.Log.Logger hiding ( logM )
import Text.Printf


-- Format the time right now given a formatting string
formattedDate :: String -> IO String
formattedDate formatString =
   liftM (formatTime defaultTimeLocale formatString)
      $ getCurrentTime >>= utcToLocalZonedTime


{- Log a message to the root logger with timestamp and priority
   included
-}
logM :: Priority -> String -> IO ()
logM pri msg = do
   fd <- formattedDate "%Y-%m-%d %H:%M:%S %Z"
   let fullMsg = printf "%s %s> %s" fd (show pri) msg
   L.logM rootLoggerName pri fullMsg


{- Default hslogger behavior sends logging to stderr. In this case,
   we want it to go to stdout.
-}
initLogging :: ConfMap -> IO ()
initLogging conf = do
   let logPath = fromJust $ lookup "logPath" conf
   let level = read $ fromJust $ lookup "logPriority" conf
   logHandler <- fileHandler logPath level
   updateGlobalLogger rootLoggerName (setHandlers [logHandler])
   updateGlobalLogger rootLoggerName (setLevel level)
