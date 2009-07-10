-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Control.Monad ( forever )
import Network.Socket
import System.IO
import System.Log

import Uacpid.Conf
import Uacpid.Log ( initLogging, logM )


main :: IO ()
main = do
   conf <- getConf
   initLogging conf

   logM NOTICE "uacpid daemon started"

   -- Open the UNIX domain socket
   s <- socket AF_UNIX Stream defaultProtocol
   connect s $ SockAddrUnix "/var/run/acpid.socket"

   -- Turn it into an ordinary handle
   hdl <- socketToHandle s ReadMode
   hSetBuffering hdl LineBuffering

   -- Read lines from it
   forever $ do
      line <- hGetLine hdl
      logM INFO $ "received from acpid: " ++ line

   -- FIXME This never gets called when we ctrl-c
   logM NOTICE "uacpid daemon stopped"
