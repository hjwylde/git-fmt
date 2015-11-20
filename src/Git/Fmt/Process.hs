
{-|
Module      : Git.Fmt.Process
Description : System process utilities.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

System process utilities.
-}

{-# LANGUAGE TemplateHaskell #-}

module Git.Fmt.Process (
    -- * Run
    runProcess, runProcess_, runCommand, runCommand_, runCreateProcess, runCreateProcess_,
) where

import Control.Monad.IO.Class
import Control.Monad.Logger

import Data.Text hiding (unwords)

import Git.Fmt.Exit

import           System.Exit
import           System.Process (CmdSpec (..), CreateProcess)
import qualified System.Process as System

-- | Runs the given executable with the arguments.
--   Returns the exit code, stdout and stderr.
runProcess :: (MonadIO m, MonadLogger m) => FilePath -> [String] -> m (ExitCode, String, String)
runProcess cmd args = runCreateProcess (System.proc cmd args) ""

-- | Runs the given executable with the arguments.
--   Depending on the exit code, either logs the stderr and exits fast (128) or returns the stdout.
runProcess_ :: (MonadIO m, MonadLogger m) => FilePath -> [String] -> m String
runProcess_ cmd args = runCreateProcess_ (System.proc cmd args) ""

-- | Runs the given command.
--   Returns the exit code, stdout and stderr.
runCommand :: (MonadIO m, MonadLogger m) => String -> m (ExitCode, String, String)
runCommand cmd = runCreateProcess (System.shell cmd) ""

-- | Runs the given command.
--   Depending on the exit code, either logs the stderr and exits fast (128) or returns the stdout.
runCommand_ :: (MonadIO m, MonadLogger m) => String -> m String
runCommand_ cmd = runCreateProcess_ (System.shell cmd) ""

-- | Runs the given 'CreateProcess'.
--   Returns the exit code, stdout and stderr.
runCreateProcess :: (MonadIO m, MonadLogger m) => CreateProcess -> String -> m (ExitCode, String, String)
runCreateProcess process stdin = do
    $(logDebug) $ pack (case System.cmdspec process of
        ShellCommand cmd    -> cmd
        RawCommand cmd args -> unwords (cmd:args)
        )

    liftIO $ System.readCreateProcessWithExitCode process stdin

-- | Runs the given 'CreateProcess'.
--   Depending on the exit code, either logs the stderr and exits fast (128) or returns the stdout.
runCreateProcess_ :: (MonadIO m, MonadLogger m) => CreateProcess -> String -> m String
runCreateProcess_ process stdin = do
    (exitCode, stdout, stderr) <- runCreateProcess process stdin

    if exitCode == ExitSuccess
        then return stdout
        else panic stderr

