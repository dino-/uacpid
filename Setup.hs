#! /usr/bin/env runhaskell

-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Control.Monad
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import System.Cmd
import System.Directory
import System.Exit
import System.FilePath


copyManDestDir :: CopyFlags -> LocalBuildInfo -> FilePath
copyManDestDir copyFlags localBuildInfo = destDir
   where
      Flag (CopyTo copyPrefix) = copyDest copyFlags

      instPrefix = fromPathTemplate $ prefix
         $ installDirTemplates localBuildInfo

      {- Can't use </> here because instPrefix is often absolute
         and </> returns the second path if ALONE if it's absolute.
         Safer to just have the extra / if that's how it goes.
      -}
      destDir = copyPrefix ++ "/" ++ instPrefix
         </> "share" </> "man" </> "man1"


instManDestDir :: LocalBuildInfo -> FilePath
instManDestDir localBuildInfo = destDir
   where
      {- There's a "right way" to get the absolute mandir FilePath 
         from the Cabal API, but I can't figure it out yet.
         Have to revisit someday.
      -}
      instPrefix = fromPathTemplate $ prefix
         $ installDirTemplates localBuildInfo

      destDir = instPrefix </> "share" </> "man" </> "man1"


copyManPage :: FilePath -> IO ()
copyManPage destDir = do
   -- Construct src dir for copying the man page
   let manFile = "uacpid.1"
   let srcPath = "resources" </> "man" </> manFile

   createDirectoryIfMissing True destDir

   putStrLn $ "Installing man page in " ++ destDir

   let destPath = destDir </> manFile

   -- Copy it to the appropriate man directory
   copyFile srcPath destPath

   -- gzip the man page
   gzipExitCode <- system $ "gzip -f " ++ destPath

   unless (gzipExitCode == ExitSuccess) $
      putStrLn "Copy of man page failed!"

   return ()


main = defaultMainWithHooks (simpleUserHooks 
   { postCopy = customPostCopy
   , postInst = customPostInst
   } )
   where
      customPostCopy _ copyFlags _ localBuildInfo =
         copyManPage (copyManDestDir copyFlags localBuildInfo)
      customPostInst _ _ _ localBuildInfo =
         copyManPage (instManDestDir localBuildInfo)
