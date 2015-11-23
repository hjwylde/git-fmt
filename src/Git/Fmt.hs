
{-|
Module      : Git.Fmt
Description : Options and handler for the git-fmt command.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Options and handler for the git-fmt command.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Git.Fmt (
    -- * Options
    Options(..), Chatty(..), Mode(..),

    -- * Handle
    handle,
) where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad.Catch    (MonadMask)
import           Control.Monad.Extra
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Parallel (MonadParallel)
import qualified Control.Monad.Parallel as Parallel
import           Control.Monad.Reader

import           Data.List.Extra   (chunksOf, linesBy, lower, nub)
import           Data.Maybe        (fromMaybe)
import qualified Data.Text         as T
import           Data.Yaml         (prettyPrintParseException)
import           Data.Yaml.Include (decodeFileEither)

import Git.Fmt.Config  as Config
import Git.Fmt.Exit
import Git.Fmt.Process

import Prelude hiding (read)

import System.Directory.Extra hiding (withCurrentDirectory)
import System.Exit
import System.FilePath
import System.IO.Temp

-- | Options.
data Options = Options {
        optChatty     :: Chatty,
        optNull       :: Bool,
        optNumThreads :: Maybe Int,
        optMode       :: Mode,
        argPaths      :: [FilePath]
    }
    deriving (Eq, Show)

-- | Chattyness level.
data Chatty = Default | Quiet | Verbose
    deriving (Eq, Show)

-- | Run mode.
data Mode = Normal | DryRun
    deriving (Eq, Show)

-- | Builds the files according to the options.
handle :: (MonadIO m, MonadLogger m, MonadMask m, MonadParallel m) => Options -> m ()
handle options = do
    gitDir      <- findGitDirectory
    filePaths   <- fmap (nub . concat) $ paths gitDir >>= mapM
        (\path -> ifM (liftIO $ doesDirectoryExist path)
            (liftIO $ listFilesRecursive path)
            (return [path])
            )
    numThreads  <- liftIO getNumCapabilities >>= \numCapabilities ->
        return $ fromMaybe numCapabilities (optNumThreads options)

    unlessM (liftIO . doesFileExist $ gitDir </> Config.defaultFileName) $ panic (gitDir </> Config.defaultFileName ++ ": not found")

    config <- liftIO (decodeFileEither $ gitDir </> Config.defaultFileName) >>= \ethr -> case ethr of
        Left error      -> panic $ gitDir </> Config.defaultFileName ++ ": error\n" ++ prettyPrintParseException error
        Right config    -> return config

    let supportedFilePaths = filter (supported config . T.pack . drop 1 . lower . takeExtension) filePaths

    flip runReaderT config . withSystemTempDirectory "git-fmt" $ \tmpDir ->
        Parallel.sequence_ . map sequence . nChunks numThreads . flip map supportedFilePaths $ \filePath -> ifM (liftIO $ doesFileExist filePath)
            (fmt options filePath (tmpDir </> filePath))
            ($(logWarn) $ T.pack (filePath ++ ": not found"))
    where
        paths gitDir
            | null (argPaths options)   = linesBy (== '\0') <$> runProcess_ "git" ["ls-files", "-z", gitDir]
            | optNull options           = return $ concatMap (linesBy (== '\0')) (argPaths options)
            | otherwise                 = return $ argPaths options
        nChunks n xs = chunksOf (maximum [1, length xs `div` n]) xs

fmt :: (MonadIO m, MonadLogger m, MonadReader Config m) => Options -> FilePath -> FilePath -> m ()
fmt options filePath tmpFilePath = do
    config <- ask
    let program = unsafeProgramFor config (T.pack . drop 1 $ takeExtension filePath)

    (exitCode, _, stderr) <- runProgram program filePath tmpFilePath
    if exitCode == ExitSuccess
        then diff options filePath tmpFilePath
        else $(logWarn) (T.pack $ filePath ++ ": error") >>
             $(logDebug) (T.pack stderr)

diff :: (MonadIO m, MonadLogger m) => Options -> FilePath -> FilePath -> m ()
diff options filePath tmpFilePath = do
    (exitCode, _, stderr) <- runProcess "diff" [filePath, tmpFilePath]
    case exitCode of
        ExitSuccess     -> $(logDebug) $ T.pack (filePath ++ ": pretty")
        ExitFailure 1   -> action filePath tmpFilePath
        _               -> $(logWarn) $ T.pack stderr
    where
        action = case optMode options of
            Normal -> normal
            DryRun -> dryRun

normal :: (MonadIO m, MonadLogger m) => FilePath -> FilePath -> m ()
normal filePath tmpFilePath = do
    $(logInfo) $ T.pack (filePath ++ ": prettified")

    liftIO $ renameFile tmpFilePath filePath

dryRun :: (MonadIO m, MonadLogger m) => FilePath -> FilePath -> m ()
dryRun filePath _ = $(logInfo) $ T.pack (filePath ++ ": ugly")

findGitDirectory :: (MonadIO m, MonadLogger m) => m String
findGitDirectory = do
    (exitCode, stdout, _) <- runProcess "git" ["rev-parse", "--show-toplevel"]

    if exitCode == ExitSuccess
        then return $ init stdout
        else panic ".git/: not found"

runProgram :: (MonadIO m, MonadLogger m) => Program -> FilePath -> FilePath -> m (ExitCode, String, String)
runProgram program inputFilePath tmpFilePath = do
    liftIO $ createDirectoryIfMissing True (takeDirectory tmpFilePath)

    runCommand . T.unpack $ substitute (T.concat [command program, inputSuffix, outputSuffix]) [
        (inputVariableName, T.pack $ '"':inputFilePath ++ "\""),
        (outputVariableName, T.pack $ '"':tmpFilePath ++ "\"")
        ]
    where
        inputSuffix
            | usesInputVariable (command program)   = T.empty
            | otherwise                             = T.pack " < " `T.append` inputVariableName
        outputSuffix
            | usesOutputVariable (command program)  = T.empty
            | otherwise                             = T.pack " > " `T.append` outputVariableName

