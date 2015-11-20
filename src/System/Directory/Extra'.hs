
{-|
Module      : System.Directory.Extra'
Description : Extra extra directory utilities.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Extra extra directory utilities.
-}

module System.Directory.Extra' (
    -- * Changing directories
    withCurrentDirectory,
) where

import Control.Monad.Catch    (MonadMask, bracket)
import Control.Monad.IO.Class

import System.Directory

-- | @withCurrentDirectory dir action@ performs @action@ with the current directory set to @dir@.
--   The current directory is reset back to what it was afterwards.
withCurrentDirectory :: (MonadIO m, MonadMask m) => FilePath -> m a -> m a
withCurrentDirectory dir action = bracket (liftIO getCurrentDirectory) (liftIO . setCurrentDirectory) $ \_ -> liftIO (setCurrentDirectory dir) >> action

