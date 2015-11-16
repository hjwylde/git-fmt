
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
    Options(..), Mode(..),

    -- * Handle
    handle,
) where

import Control.Monad.Catch      (MonadMask, bracket)
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Logger

import Data.Text (pack)

import Git.Fmt.Language
import Git.Fmt.Process

import Prelude hiding (read)

import System.Directory
import System.FilePath

import Text.Parsec


-- | Options.
data Options = Options {
        optQuiet        :: Bool,
        optVerbose      :: Bool,
        optMode         :: Mode,
        argFilePaths    :: [FilePath]
    }
    deriving (Eq, Show)

-- | Run mode.
data Mode = Normal | DryRun
    deriving (Eq, Show)


-- | Builds the files according to the options.
handle :: (MonadIO m, MonadLogger m, MonadMask m) => Options -> m ()
handle options = run "git" ["rev-parse", "--show-toplevel"] >>= \dir -> withCurrentDirectory (init dir) $ do
    filePaths' <- filePaths

    forM_ filePaths' $ \filePath ->
        whenJust (languageOf $ takeExtension filePath) $ \language ->
            ifM (liftIO $ doesFileExist filePath)
                (fmt options filePath language)
                ($(logWarn) $ pack (filePath ++ ": not found"))
    where
        filePaths
            | null (argFilePaths options)   = lines <$> run "git" ["ls-files"]
            | otherwise                     = return $ argFilePaths options

fmt :: (MonadIO m, MonadLogger m) => Options -> FilePath -> Language -> m ()
fmt options filePath language = do
    (ugly, mPretty) <- read filePath language

    whenJust mPretty $ \pretty -> if ugly == pretty
        then $(logDebug) $ pack (filePath ++ ": pretty")
        else action filePath ugly pretty
    where
        action = case optMode options of
            Normal -> normal
            DryRun -> dryRun

normal :: (MonadIO m, MonadLogger m) => FilePath -> String -> String -> m ()
normal filePath _ pretty = do
    $(logInfo) $ pack (filePath ++ ": prettified")

    liftIO $ writeFile filePath pretty

dryRun :: (MonadIO m, MonadLogger m) => FilePath -> String -> String -> m ()
dryRun filePath _ _ = $(logInfo) $ pack (filePath ++ ": ugly")

read :: (MonadIO m, MonadLogger m) => FilePath -> Language -> m (String, Maybe String)
read filePath language = do
    input <- liftIO $ readFile filePath

    case runParser (parser language) () filePath input of
        Left error  -> do
            $(logWarn)  $ pack (filePath ++ ": parse error")
            $(logDebug) $ pack (show error)

            return (input, Nothing)
        Right doc   -> return (input, Just $ renderWithTabs doc)

withCurrentDirectory :: (MonadIO m, MonadMask m) => FilePath -> m a -> m a
withCurrentDirectory dir action = bracket (liftIO getCurrentDirectory) (liftIO . setCurrentDirectory) $ \_ -> liftIO (setCurrentDirectory dir) >> action

