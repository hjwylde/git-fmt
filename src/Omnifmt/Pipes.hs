
{-|
Module      : Omnifmt.Pipes
Description : Pipeline for formatting files.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Pipeline for formatting files.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Omnifmt.Pipes (
    -- * Filters
    filterFileSupported, filterFileExists,

    -- * Transformers
    zipTemporaryFilePath, runProgram, runDiff,

    -- * Consumers
    formatter, dryRunner,
) where

import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.Logger
import Control.Monad.Reader

import qualified Data.Text as T

import Omnifmt.Config
import Omnifmt.Process

import           Pipes
import qualified Pipes.Prelude as Pipes

import System.Directory.Extra
import System.Exit
import System.FilePath

-- | Filters files that have languages supported by the config.
filterFileSupported :: (MonadIO m, MonadReader Config m) => Pipe FilePath FilePath m ()
filterFileSupported = ask >>= \config -> Pipes.filter (supported config . T.toLower . T.pack . drop 1 . takeExtension)

-- | Filters files that exist.
filterFileExists :: (MonadIO m, MonadLogger m) => Pipe FilePath FilePath m ()
filterFileExists = forever $ await >>= \filePath -> ifM (liftIO $ doesFileExist filePath)
    (yield filePath) (lift . logWarnN . T.pack $ filePath ++ ": not found")

-- | Zips the file path with a temporary file path.
--   The temporary file path is created by concatenating the given directory with the file path.
zipTemporaryFilePath :: MonadIO m => FilePath -> Pipe FilePath (FilePath, FilePath) m ()
zipTemporaryFilePath tmpDir = Pipes.mapM $ \filePath -> do
    liftIO $ createDirectoryIfMissing True (takeDirectory $ tmpDir </> filePath)

    return (filePath, tmpDir </> filePath)

runProgram :: (MonadIO m, MonadLogger m, MonadReader Config m) => Pipe (FilePath, FilePath) (FilePath, FilePath) m ()
runProgram = Pipes.filterM $ \(uglyFilePath, prettyFilePath) -> do
    config <- ask
    let program = unsafeProgramFor config (T.pack . drop 1 $ takeExtension uglyFilePath)

    (exitCode, _, stderr) <- runCommand . T.unpack $ substitute (T.concat [command program, inputSuffix program, outputSuffix program]) [
        (inputVariableName, T.pack uglyFilePath),
        (outputVariableName, T.pack prettyFilePath)
        ]
    if exitCode == ExitSuccess
        then return True
        else logWarnN (T.pack $ uglyFilePath ++ ": error") >>
             logDebugN (T.pack stderr) >>
             return False
    where
        inputSuffix program
            | usesInputVariable (command program)   = T.empty
            | otherwise                             = T.pack " < " `T.append` inputVariableName
        outputSuffix program
            | usesOutputVariable (command program)  = T.empty
            | otherwise                             = T.pack " > " `T.append` outputVariableName

runDiff :: (MonadIO m, MonadLogger m) => Pipe (FilePath, FilePath) (FilePath, FilePath) m ()
runDiff = Pipes.filterM $ \(uglyFilePath, prettyFilePath) -> do
    (exitCode, _, stderr) <- runProcess "diff" [uglyFilePath, prettyFilePath]

    case exitCode of
        ExitFailure 1   -> return True
        ExitSuccess     -> logDebugN (T.pack $ uglyFilePath ++ ": pretty") >> return False
        _               -> logWarnN (T.pack stderr) >> return False

formatter :: (MonadIO m, MonadLogger m) => Consumer (FilePath, FilePath) m ()
formatter = Pipes.mapM_ $ \(uglyFilePath, prettyFilePath) -> do
    logInfoN $ T.pack (uglyFilePath ++ ": prettified")

    liftIO $ renameFile prettyFilePath uglyFilePath

dryRunner :: (MonadIO m, MonadLogger m) => Consumer (FilePath, FilePath) m ()
dryRunner = Pipes.mapM_ $ \(uglyFilePath, _) -> logInfoN (T.pack $ uglyFilePath ++ ": ugly")

