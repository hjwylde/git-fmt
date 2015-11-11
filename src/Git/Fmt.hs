
{-|
Module      : Git.Fmt
Description : Options and handler for the git-fmt command.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Options and handler for the git-fmt command.
-}

module Git.Fmt (
    -- * Options
    Options(..),

    -- * Handle
    handle,
) where

import Control.Monad
import Control.Monad.Catch      (MonadMask, bracket)
import Control.Monad.IO.Class
import Control.Monad.Logger

import Data.List (intersect)

import Git.Fmt.Language
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
handle options = run "git" ["rev-parse", "--show-toplevel"] >>= \dir -> withCurrentDirectory (init dir) $ do
    files <- filter ((`elem` extensions) . takeExtension) . lines <$> run "git" ["ls-files"]

    forM_ files $ liftIO . putStrLn
    where
        extensions
            | null $ optExtensions options  = supportedExtensions
            | otherwise                     = optExtensions options `intersect` supportedExtensions

withCurrentDirectory :: (MonadIO m, MonadMask m) => FilePath -> m a -> m a
withCurrentDirectory dir action = bracket (liftIO getCurrentDirectory) (liftIO . setCurrentDirectory) $ \_ -> liftIO (setCurrentDirectory dir) >> action

