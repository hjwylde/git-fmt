
{-|
Module      : Git.Fmt
Description : Options and handler for the git-fmt command.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Options and handler for the git-fmt command.
-}

{-# LANGUAGE TemplateHaskell #-}

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

import Data.Text (pack)

import Git.Fmt.Language
import Git.Fmt.Process

import System.Directory
import System.FilePath

import Text.Parsec


-- | Options.
data Options = Options {}
    deriving (Eq, Show)

-- | Builds the files according to the options.
handle :: (MonadIO m, MonadLogger m, MonadMask m) => Options -> m ()
handle _ = run "git" ["rev-parse", "--show-toplevel"] >>= \dir -> withCurrentDirectory (init dir) $ do
    filePaths <- lines <$> run "git" ["ls-files"]

    forM_ filePaths $ \filePath ->
        maybe (return ()) (`fmt` filePath) (languageOf $ takeExtension filePath)


fmt :: (MonadIO m, MonadLogger m) => Language -> FilePath -> m ()
fmt language filePath = do
    input <- liftIO $ readFile filePath

    case runParser (parser language) () filePath input of
        Left error  -> $(logWarn) $ pack (show error)
        Right doc   -> liftIO $ putStrLn (renderWithTabs doc)

withCurrentDirectory :: (MonadIO m, MonadMask m) => FilePath -> m a -> m a
withCurrentDirectory dir action = bracket (liftIO getCurrentDirectory) (liftIO . setCurrentDirectory) $ \_ -> liftIO (setCurrentDirectory dir) >> action

