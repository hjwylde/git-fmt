
{-|
Module      : Omnifmt.Pipes
Description : Pipeline for formatting files.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Pipeline for formatting files.
The functions listed here are in order of the recommended chain, but it is possible to mix and match
    them or add custom functions inbetween.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}

module Omnifmt.Pipes (
    -- * Status
    Status(..),

    -- * Producers
    omnifmt,

    -- * Transformers
    select, checkFileSupported, checkFileExists, runProgram, checkFilePretty, commit,

    -- * Consumers
    statusPrinter,
) where

import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.Logger
import Control.Monad.Reader

import           Data.List.Extra  (lower)
import qualified Data.Text        as T
import           Data.Tuple.Extra (fst3)

import Omnifmt.Config
import Omnifmt.Process

import           Pipes
import qualified Pipes.Prelude as Pipes

import System.Directory.Extra
import System.Exit
import System.FilePath

-- | A status for a file going through the omnifmt pipeline.
data Status = Unknown       -- ^ The file has not been processed.
            | Error         -- ^ An error occurred somewhere.
            | Unsupported   -- ^ The file type is unsupported (i.e., no applicable 'Program').
            | NotFound      -- ^ The file could not be found.
            | Pretty        -- ^ The file is pretty.
            | Ugly          -- ^ The file is ugly.
            | Prettified    -- ^ The file is now pretty.
    deriving (Eq, Show)

-- | Takes an input (ugly) file path and an empty output file path and prepends the 'Unknown' status
--   to them.
omnifmt :: FilePath -> FilePath -> (Status, FilePath, FilePath)
omnifmt = (Unknown,,)

-- | Utility method for applying a function to files that match certain statuses.
--   Any files that don't match the given statuses will be passed through unmodified.
select :: Monad m => [Status] -> ((Status, FilePath, FilePath) -> m (Status, FilePath, FilePath)) -> Pipe (Status, FilePath, FilePath) (Status, FilePath, FilePath) m ()
select states action = Pipes.mapM (\item -> if fst3 item `elem` states then action item else return item)

-- | Checks all 'Unknown' files to see if they're 'supported'.
checkFileSupported :: MonadReader Config m => Pipe (Status, FilePath, FilePath) (Status, FilePath, FilePath) m ()
checkFileSupported = select [Unknown] $ \item@(_, uglyFilePath, prettyFilePath) ->
    ask >>= \config -> if supported config . T.toLower . T.pack . drop 1 $ takeExtension uglyFilePath
        then return item
        else return (Unsupported, uglyFilePath, prettyFilePath)

-- | Checks all 'Unknown' ugly file paths to see if they exist.
checkFileExists :: MonadIO m => Pipe (Status, FilePath, FilePath) (Status, FilePath, FilePath) m ()
checkFileExists = select [Unknown] $ \item@(_, uglyFilePath, prettyFilePath) ->
    ifM (liftIO $ doesFileExist uglyFilePath)
        (return item)
        (return (NotFound, uglyFilePath, prettyFilePath))

-- | Runs the applicable 'Program''s command on all 'Unknown' files.
--   This reads in the ugly file path and writes out to the pretty file path.
--
--   Note that this function assumes that the file is supported, so make sure the file has been
--   piped through 'checkFileSupported' first.
--
--   Any errors that occur are logged using 'logDebugN'.
runProgram :: (MonadIO m, MonadLogger m, MonadReader Config m) => Pipe (Status, FilePath, FilePath) (Status, FilePath, FilePath) m ()
runProgram = select [Unknown] $ \item@(_, uglyFilePath, prettyFilePath) -> do
    config <- ask
    let program = unsafeProgramFor config (T.pack . drop 1 $ takeExtension uglyFilePath)

    (exitCode, _, stderr) <- runTimedCommand 10 . T.unpack $ substitute (T.concat [command program, inputSuffix program, outputSuffix program]) [
        (inputVariableName, T.pack uglyFilePath),
        (outputVariableName, T.pack prettyFilePath)
        ]

    if exitCode == ExitSuccess
        then return item
        else logDebugN (T.pack stderr) >>
             return (Error, uglyFilePath, prettyFilePath)
    where
        inputSuffix program
            | usesInputVariable (command program)   = T.empty
            | otherwise                             = T.pack " < " `T.append` inputVariableName
        outputSuffix program
            | usesOutputVariable (command program)  = T.empty
            | otherwise                             = T.pack " > " `T.append` outputVariableName

-- | Runs a diff over the two file paths for all 'Unknown' files.
--
--   This function always updates the status to either 'Ugly', 'Pretty' or 'Error'.
--
--   Any errors that occur are logged using 'logDebugN'.
checkFilePretty :: (MonadIO m, MonadLogger m) => Pipe (Status, FilePath, FilePath) (Status, FilePath, FilePath) m ()
checkFilePretty = select [Unknown] $ \(_, uglyFilePath, prettyFilePath) -> do
    (exitCode, _, stderr) <- runProcess "diff" [uglyFilePath, prettyFilePath]

    case exitCode of
        ExitFailure 1   -> return (Ugly, uglyFilePath, prettyFilePath)
        ExitSuccess     -> return (Pretty, uglyFilePath, prettyFilePath)
        _               -> logDebugN (T.pack stderr) >>
                           return (Error, uglyFilePath, prettyFilePath)

-- | Commits the result of 'runProgram'.
--   I.e., writes over all 'Ugly' files with their corresponding pretty file.
--
--   This function updates the status to 'Prettified'.
commit :: MonadIO m => Pipe (Status, FilePath, FilePath) (Status, FilePath, FilePath) m ()
commit = select [Ugly] $ \(_, uglyFilePath, prettyFilePath) -> do
    liftIO $ renameFile prettyFilePath uglyFilePath

    return (Prettified, uglyFilePath, prettyFilePath)

-- | Logs the status of each file.
--   'Unsupported', 'NotFound' and 'Pretty' are logged using 'logDebugN'.
--   'Ugly' and 'Prettified' are logged using 'logInfoN'.
--   'Unknown' and 'Error' are logged using 'logErrorN'.
statusPrinter :: MonadLogger m => Consumer (Status, FilePath, FilePath) m ()
statusPrinter = Pipes.mapM_ $ \(status, uglyFilePath, _) ->
    logFunction status (T.pack $ uglyFilePath ++ ": " ++ showStatus status)
    where
        logFunction Unknown     = logWarnN
        logFunction Unsupported = logDebugN
        logFunction NotFound    = logDebugN
        logFunction Error       = logWarnN
        logFunction Pretty      = logDebugN
        logFunction Ugly        = logInfoN
        logFunction Prettified  = logInfoN

        showStatus NotFound = "not found"
        showStatus status   = lower $ show status

