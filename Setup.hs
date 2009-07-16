#! /usr/bin/env runhaskell

-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

import Control.Monad
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import System.Cmd
import System.Directory
import System.Exit
import System.FilePath


main = defaultMainWithHooks (simpleUserHooks 
   { postCopy = copyManPage
   , postInst = copyManPage
   } )
   where
      copyManPage _ _ _ localBuildInfo = do
         -- Construct src and dest dirs for copying the man page
         let manFile = "uacpid.1"
         let srcPath = "resources" </> "man" </> manFile

         {- There's a "right way" to get the absolute mandir FilePath 
            from the Cabal API, but I can't figure it out yet.
            Have to revisit someday.
         -}
         let prefixDir = fromPathTemplate $ prefix
               $ installDirTemplates localBuildInfo
         let destDir =
               prefixDir </> "share" </> "man" </> "man1"

         createDirectoryIfMissing True destDir

         putStrLn $ "Installing man page in " ++ destDir

         let destPath = destDir </> manFile

         -- Copy it to the appropriate man directory, based on the
         -- prefix
         copyFile srcPath destPath

         -- gzip the man page
         gzipExitCode <- system $ "gzip -f " ++ destPath

         unless (gzipExitCode == ExitSuccess) $
            putStrLn "Copy of man page failed!"

         return ()
