
{-|
Module      : Git.Fmt
Description : Options and handler for the git-fmt command.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Options and handler for the git-fmt command.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Git.Fmt (
    -- * Options
    Options(..), Chatty(..), Mode(..),

    -- * Handle
    handle,
) where

import Control.Monad.Catch      (MonadMask)
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader

import qualified    Data.Text           as T
import              Data.List.Extra     (linesBy, lower, nub, replace)
import              Data.Yaml           (prettyPrintParseException)
import              Data.Yaml.Include   (decodeFileEither)

import Git.Fmt.Config as Config
import Git.Fmt.Process

import Prelude hiding (read)

import System.Directory.Extra   hiding (withCurrentDirectory)
import System.Directory.Extra'
import System.Exit
import System.FilePath
import System.IO.Temp
import System.IO.Extra'

-- | Options.
data Options = Options {
        optChatty   :: Chatty,
        optNull     :: Bool,
        optMode     :: Mode,
        argPaths    :: [FilePath]
    }
    deriving (Eq, Show)

-- | Chattyness level.
data Chatty = Default | Quiet | Verbose
    deriving (Eq, Show)

-- | Run mode.
data Mode = Normal | DryRun
    deriving (Eq, Show)

-- | Builds the files according to the options.
handle :: (MonadIO m, MonadLogger m, MonadMask m) => Options -> m ()
handle options = findTopLevelGitDirectory >>= \dir -> withCurrentDirectory (init dir) $ do
    filePaths <- fmap (nub . concat) $ paths >>= mapM
        (\path -> ifM (liftIO $ doesDirectoryExist path)
            (liftIO $ listFilesRecursive path)
            (return [path])
            )

    unlessM (liftIO $ doesFileExist Config.fileName) $ panic (Config.fileName ++ ": not found")

    config <- liftIO (decodeFileEither Config.fileName) >>= \ethr -> case ethr of
        Left error      -> panic $ Config.fileName ++ ": error\n" ++ prettyPrintParseException error
        Right config    -> return config

    let supportedFilePaths = filter (supported config . T.pack . drop 1 . lower . takeExtension) filePaths

    flip runReaderT config $ withSystemTempDirectory "git-fmt" $ \tmpDir ->
        forM_ supportedFilePaths $ \filePath -> ifM (liftIO $ doesFileExist filePath)
            (fmt options filePath (tmpDir </> filePath))
            ($(logWarn) $ T.pack (filePath ++ ": not found"))
    where
        paths
            | null (argPaths options)   = linesBy (== '\0') <$> runProcess_ "git" ["ls-files", "-z"]
            | optNull options           = return $ concatMap (linesBy (== '\0')) (argPaths options)
            | otherwise                 = return $ argPaths options

fmt :: (MonadIO m, MonadLogger m, MonadReader Config m) => Options -> FilePath -> FilePath -> m ()
fmt options filePath tmpFilePath = do
    config <- ask
    let program = unsafeProgramFor config (T.pack . drop 1 $ takeExtension filePath)

    runProgram_ program filePath tmpFilePath

    (exitCode, _, stderr) <- runProcess "diff" [filePath, tmpFilePath]
    case exitCode of
        ExitSuccess     -> $(logDebug) $ T.pack (filePath ++ ": pretty")
        ExitFailure 1   -> action filePath tmpFilePath
        _               -> $(logWarn) $ T.pack stderr
    where
        action = case optMode options of
            Normal -> fmtNormal
            DryRun -> fmtDryRun

fmtNormal :: (MonadIO m, MonadLogger m) => FilePath -> FilePath -> m ()
fmtNormal filePath tmpFilePath = do
    $(logInfo) $ T.pack (filePath ++ ": prettified")

    liftIO $ renameFile tmpFilePath filePath

fmtDryRun :: (MonadIO m, MonadLogger m) => FilePath -> FilePath -> m ()
fmtDryRun filePath _ = $(logInfo) $ T.pack (filePath ++ ": ugly")

findTopLevelGitDirectory :: (MonadIO m, MonadLogger m) => m String
findTopLevelGitDirectory = do
    (exitCode, stdout, _) <- runProcess "git" ["rev-parse", "--show-toplevel"]

    if exitCode == ExitSuccess
        then return stdout
        else panic ".git/: not found"

runProgram_ :: (MonadIO m, MonadLogger m) => Program -> FilePath -> FilePath -> m ()
runProgram_ program inputFilePath tmpFilePath = do
    liftIO $ createDirectoryIfMissing True (takeDirectory tmpFilePath)

    void . runCommand_ $ foldr (uncurry replace) (T.unpack $ command program) [
        ("{{inputFilePath}}", inputFilePath),
        ("{{tmpFilePath}}", tmpFilePath)
        ]

