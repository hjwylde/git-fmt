
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
import Control.Monad.IO.Class
import Control.Monad.Logger

import Data.Text (pack)

import Git.Fmt.Language
import Git.Fmt.Process

import System.Directory
import System.FilePath

import Text.Parsec


-- | Options.
data Options = Options {
        optQuiet        :: Bool,
        optVerbose      :: Bool,
        optDryRun       :: Bool,
        optListUgly     :: Bool,
        argFilePaths    :: [FilePath]
    }
    deriving (Eq, Show)

-- | Builds the files according to the options.
handle :: (MonadIO m, MonadLogger m) => Options -> m ()
handle options = do
    gitDir      <- init <$> run "git" ["rev-parse", "--show-toplevel"]
    filePaths   <- if null (argFilePaths options) then lines <$> run "git" ["ls-files", gitDir] else return (argFilePaths options)

    filterM (liftIO . doesFileExist) filePaths >>= mapM_ (\filePath ->
        maybe (return ()) (fmt options filePath) (languageOf $ takeExtension filePath))


fmt :: (MonadIO m, MonadLogger m) => Options -> FilePath -> Language -> m ()
fmt options filePath language = do
    input <- liftIO $ readFile filePath

    case runParser (parser language) () filePath input of
        Left error  -> do
            $(logWarn)  $ pack (filePath ++ ": parse error")
            $(logDebug) $ pack (show error)
        Right doc   -> do
            let output = renderWithTabs doc

            if input == output
                then
                    $(logDebug) $ pack (filePath ++ ": pretty")
                else do
                    $(logInfo) $ pack (filePath ++ ": ugly" ++ if optDryRun options then "" else " (-> pretty)")

                    unless (optDryRun options) $ liftIO (writeFile filePath output)

