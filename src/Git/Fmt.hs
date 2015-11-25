
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

module Git.Fmt (
    -- * Options
    Options(..), Chatty(..), Mode(..),

    -- * Handle
    handle,
) where

import Control.Applicative
import Control.Monad.Catch    (MonadMask)
import Control.Monad.Extra
import Control.Monad.IO.Class
import Control.Monad.Logger
import Control.Monad.Reader

import Data.List.Extra  (linesBy)
import Data.Tuple.Extra (fst3)

import Git.Fmt.Config  as Config
import Git.Fmt.Exit
import Git.Fmt.Pipes
import Git.Fmt.Process

import           Pipes
import qualified Pipes.Prelude as Pipes
import           Prelude       hiding (read)

import System.Directory.Extra
import System.Exit
import System.IO.Temp

-- | Options.
data Options = Options {
        optChatty  :: Chatty,
        optNull    :: Bool,
        optMode    :: Mode,
        optThreads :: Maybe Int,
        argPaths   :: [FilePath]
    }
    deriving (Eq, Show)

-- | Chattyness level.
data Chatty = Default | Quiet | Verbose
    deriving (Eq, Show)

-- | Run mode.
data Mode = Normal | DryRun
    deriving (Eq, Show)

-- | Builds the files according to the options.
handle :: (MonadIO m, MonadLogger m, MonadMask m, MonadReader Config m) => Options -> m ()
handle options = do
    checkGitRepository

    let providedPaths   = (if optNull options then concatMap (linesBy (== '\0')) else id) $ argPaths options
    providedFilePaths   <- concatMapM expandDirectory providedPaths

    withSystemTempDirectory "git-fmt" $ \tmpDir -> runEffect $
        trackedFilePaths >->
        (if null providedPaths then cat else Pipes.filter (`elem` providedFilePaths)) >->
        filterFileSupported >->
        filterFileExists >->
        zipTemporaryFilePath tmpDir >->
        runProgram >->
        runDiff >->
        consumer
    where
        consumer = case optMode options of
            Normal  -> formatter
            DryRun  -> dryRunner

expandDirectory :: MonadIO m => FilePath -> m [FilePath]
expandDirectory path = ifM (liftIO $ doesDirectoryExist path) (liftIO $ listFilesRecursive path) (return [path])

checkGitRepository :: (MonadIO m, MonadLogger m) => m ()
checkGitRepository = do
    exitCode <- fst3 <$> runProcess "git" ["rev-parse", "--show-toplevel"]

    when (exitCode /= ExitSuccess) $ panic ".git/: not found"

