
{-|
Module      : Omnifmt.Process
Description : System process utilities.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

System process utilities.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Omnifmt.Process (
    -- * Run
    runProcess, runProcess_, runTimedProcess, runCommand, runCommand_, runTimedCommand,
    runCreateProcess, runCreateProcess_,
) where

import Control.Monad.Except
import Control.Monad.Logger

import Data.Text hiding (unwords)

import Omnifmt.Exit

import           System.Exit
import           System.Process.Extra (CmdSpec (..), CreateProcess)
import qualified System.Process.Extra as System

-- | Runs the given executable with the arguments.
--   Returns the exit code, stdout and stderr.
runProcess :: (MonadIO m, MonadLogger m) => FilePath -> [String] -> m (ExitCode, String, String)
runProcess cmd args = runCreateProcess (System.proc cmd args) ""

-- | Runs the given executable with the arguments.
--   Depending on the exit code, either logs the stderr and exits fast (128) or returns the stdout.
runProcess_ :: (MonadError ExitCode m, MonadIO m, MonadLogger m) => FilePath -> [String] -> m String
runProcess_ cmd args = runCreateProcess_ (System.proc cmd args) ""

-- | Runs the given executable with the arguments.
--   Returns the exit code, stdout and stderr.
--   The command is wrapped in a `timeout -k N*2 N` call.
runTimedProcess :: (MonadIO m, MonadLogger m) => Int -> FilePath -> [String] -> m (ExitCode, String, String)
runTimedProcess n cmd args = runCreateProcess (System.proc "timeout" $ "-k":show (n * 2):show n:cmd:args) ""

-- | Runs the given command.
--   Returns the exit code, stdout and stderr.
runCommand :: (MonadIO m, MonadLogger m) => String -> m (ExitCode, String, String)
runCommand cmd = runCreateProcess (System.shell cmd) ""

-- | Runs the given command.
--   Depending on the exit code, either logs the stderr and exits fast (128) or returns the stdout.
runCommand_ :: (MonadError ExitCode m, MonadIO m, MonadLogger m) => String -> m String
runCommand_ cmd = runCreateProcess_ (System.shell cmd) ""

-- | Runs the given command.
--   Returns the exit code, stdout and stderr.
--   The command is wrapped in a `timeout -k N*2 N` call.
runTimedCommand :: (MonadIO m, MonadLogger m) => Int -> String -> m (ExitCode, String, String)
runTimedCommand n cmd = runCreateProcess (System.shell $ unwords ["timeout -k", show $ n * 2, show n, cmd]) ""

-- | Runs the given 'CreateProcess'.
--   Returns the exit code, stdout and stderr.
runCreateProcess :: (MonadIO m, MonadLogger m) => CreateProcess -> String -> m (ExitCode, String, String)
runCreateProcess process stdin = do
    logDebugN $ pack (case System.cmdspec process of
        ShellCommand cmd    -> cmd
        RawCommand cmd args -> unwords (cmd:args)
        )

    liftIO $ System.readCreateProcessWithExitCode process stdin

-- | Runs the given 'CreateProcess'.
--   Depending on the exit code, either logs the stderr and exits fast (128) or returns the stdout.
runCreateProcess_ :: (MonadError ExitCode m, MonadIO m, MonadLogger m) => CreateProcess -> String -> m String
runCreateProcess_ process stdin = do
    (exitCode, stdout, stderr) <- runCreateProcess process stdin

    if exitCode == ExitSuccess
        then return stdout
        else panic stderr

