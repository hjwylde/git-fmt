
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
    runProcess, runCommand, runCommand_, runCreateProcess,
) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Logger

import Data.Text hiding (unwords)

import              System.Exit
import              System.Process (CreateProcess, CmdSpec(..))
import qualified    System.Process as System

-- | Runs the given command with the arguments.
--   Depending on the exit code, either logs the stderr and exits fast or returns the stdout.
runProcess :: (MonadIO m, MonadLogger m) => FilePath -> [String] -> m String
runProcess cmd args = runCreateProcess (System.proc cmd args) ""

-- | Runs the given command.
--   Depending on the exit code, either logs the stderr and exits fast or returns the stdout.
runCommand :: (MonadIO m, MonadLogger m) => String -> m String
runCommand cmd = runCreateProcess (System.shell cmd) ""

-- | Runs the given command and discards the output.
--   Depending on the exit code, either logs the stderr and exits fast or returns the stdout.
runCommand_ :: (MonadIO m, MonadLogger m) => String -> m ()
runCommand_ = void . runCommand

-- | Runs the given 'CreateProcess'.
--   Depending on the exit code, either logs the stderr and exits fast or returns the stdout.
runCreateProcess :: (MonadIO m, MonadLogger m) => CreateProcess -> String -> m String
runCreateProcess process stdin = do
    $(logDebug) $ pack (case System.cmdspec process of
        ShellCommand cmd    -> cmd
        RawCommand cmd args -> unwords (cmd:args)
        )

    (exitCode, stdout, stderr) <- liftIO $ System.readCreateProcessWithExitCode process stdin

    if exitCode == ExitSuccess
        then return stdout
        else $(logError) (pack stderr) >> liftIO (exitWith $ ExitFailure 1)

