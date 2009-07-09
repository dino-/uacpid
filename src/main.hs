-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Control.Monad ( forever )
import Network.Socket
import System.IO


main :: IO ()
main = do
   -- Open the UNIX domain socket
   s <- socket AF_UNIX Stream defaultProtocol
   connect s $ SockAddrUnix "/var/run/acpid.socket"

   -- Turn it into an ordinary handle
   hdl <- socketToHandle s ReadMode
   hSetBuffering hdl LineBuffering

   -- Read lines from it
   forever $ do
      line <- hGetLine hdl
      print line
