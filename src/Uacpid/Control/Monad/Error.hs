-- Copyright: 2009 Dino Morelli
-- License: BSD3 (see LICENSE)
-- Author: Dino Morelli <dino@ui3.info>

{-# LANGUAGE FlexibleContexts #-}

{- |
   Convenience function for turning (Maybe a) values into 
   (MonadError e a) actions plus functions for expressing Data.Map 
   lookups as MonadError actions
-}
module Uacpid.Control.Monad.Error
   ( maybeThrow
   , lookupEWith, lookupEString
   )
   where

import Control.Monad.Error
import Data.Map hiding ( map )
import Prelude hiding ( lookup )


{- |
   Turn an error value and a (Maybe a) into a (MonadError e a) action
-}
maybeThrow :: (MonadError e m) => e -> Maybe a -> m a
maybeThrow err mval = maybe (throwError err) return mval


{- |
   Look up a String key in a Map as an action in (MonadError e), 
   providing a function to transform key type k to error type e.

   See lookupEString below for a usage example.
-}
lookupEWith :: (MonadError e m, Ord k) =>
               (k -> e) -> k -> Map k a -> m a
lookupEWith f k m = maybeThrow (f k) $ lookup k m


{- |
   Look up a String key in a Map as an action in (MonadError String), 
   with a default message that the key was not found as the
   error.
-}
lookupEString :: (MonadError String m) =>
                 String -> Map String a -> m a
lookupEString =
   lookupEWith (\j -> "Key " ++ j ++ " not found")
