-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

module Uacpid.Log
   ( initLogging, logM, logTest )
   where

import Control.Monad ( liftM )
import Data.Map ( lookup )
import Data.Maybe
import Data.Time.Clock ( getCurrentTime )
import Data.Time.Format ( defaultTimeLocale, formatTime )
import Data.Time.LocalTime ( utcToLocalZonedTime )
import Prelude hiding ( lookup )
import System.Log.Handler.Simple ( fileHandler )
import qualified System.Log.Logger as L ( logM )
import           System.Log.Logger hiding ( logM )
import Text.Printf

import Uacpid.Conf


-- Format the time right now given a formatting string
formattedDate :: String -> IO String
formattedDate formatString' =
   liftM (formatTime defaultTimeLocale formatString')
      $ getCurrentTime >>= utcToLocalZonedTime


{- Log a message to the root logger with timestamp and priority
   included
-}
logM :: Priority -> String -> IO ()
logM pri msg = do
   fd <- formattedDate "%Y-%m-%d %H:%M:%S %Z"
   let fullMsg = printf "%s %7s> %s" fd (show pri) msg
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


{- Test function to generate every kind of log message we use
-}
logTest :: IO ()
logTest = do
   logM DEBUG "log test message 1 of 5"
   logM INFO "log test message 2 of 5"
   logM NOTICE "log test message 3 of 5"
   logM WARNING "log test message 4 of 5"
   logM ERROR "log test message 5 of 5"
