1.1 (2015-06-28)

   * Replaced deprecated Control.Monad.ErrorT with ExceptT
   * Fixed defaultTimeLocale import problem
   * Now getting version from cabal info using Paths_ code
   * Updated cabal info
   * Moved copyright date up to 2015
   * Adjustments to documentation


1.0.3.0 (2011-03-18)

   * Fixed code that was leaving zombie child processes by not
     waiting for them
   * Changed listenAcpi delay time from 0.25 secs to 2 secs
   * Refined developer and testing environment creation scripts
   * Updated and clarified some documentation


1.0.1 (2010-04-06)

   * Fix for a recent change in how System.Directory.doesFileExist
     behaves with sockets
   * Fixes for new GHC 6.12.x unused do binding warnings
   * Various cosmetic changes of copyright dates and version strings
   * Added HCAR info


0.0.4 (2009-07-19)

   * Another try at fixing the broken man page installation problem
     on Arch Linux.


0.0.3 (2009-07-15)

   * Fix for installation problem on Arch Linux. The man page was
     not getting installed.


0.0.2 (2009-07-15)

   * Initial release
