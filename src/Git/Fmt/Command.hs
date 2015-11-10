
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

import Control.Monad
import Control.Monad.Catch      (MonadMask, bracket)
import Control.Monad.IO.Class
import Control.Monad.Logger

import Git.Fmt.Process

import System.Directory
import System.FilePath


-- | Options.
data Options = Options {
        optExtensions :: [String]
    }
    deriving (Eq, Show)

-- | Builds the files according to the options.
handle :: (MonadIO m, MonadLogger m, MonadMask m) => Options -> m ()
handle options = run "git" ["rev-parse", "--show-toplevel"] >>= (`withCurrentDirectory` fmt options) . init


fmt :: (MonadIO m, MonadLogger m) => Options -> m ()
fmt options = do
    indexFiles <- lines <$> run "git" ["ls-files"]

    let files = if null $ optExtensions options
        then indexFiles
        else filter ((`elem` optExtensions options) . takeExtension) indexFiles

    forM_ files $ liftIO . putStrLn

withCurrentDirectory :: (MonadIO m, MonadMask m) => FilePath -> m a -> m a
withCurrentDirectory dir action = bracket (liftIO getCurrentDirectory) (liftIO . setCurrentDirectory) $ \_ -> liftIO (setCurrentDirectory dir) >> action

