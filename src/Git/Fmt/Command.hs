
{-|
Module      : Git.Fmt.Command
Description : Options and handler for the git-fmt command.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Options and handler for the git-fmt command.
-}

module Git.Fmt.Command (
    -- * Options
    Options(..),

    -- * Handle
    handle,
) where

import Control.Monad.Catch      (MonadMask, bracket)
import Control.Monad.IO.Class
import Control.Monad.Logger

import Git.Fmt.Process

import System.Directory


-- | Options.
data Options = Options {}
    deriving (Eq, Show)

-- | Builds the files according to the options.
handle :: (MonadIO m, MonadLogger m, MonadMask m) => Options -> m ()
handle _ = run "git" ["rev-parse", "--show-toplevel"] >>= (`withCurrentDirectory` fmt) . init

fmt :: (MonadIO m, MonadLogger m) => m ()
fmt = run "git" ["ls-files"] >>= liftIO . putStr

withCurrentDirectory :: (MonadIO m, MonadMask m) => FilePath -> m a -> m a
withCurrentDirectory dir action = bracket (liftIO getCurrentDirectory) (liftIO . setCurrentDirectory) $ \_ -> liftIO (setCurrentDirectory dir) >> action

