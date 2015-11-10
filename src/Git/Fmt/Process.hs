
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
    run,
) where

import Control.Monad.IO.Class
import Control.Monad.Logger

import Data.Text

import System.Exit
import System.Process as System


-- | Runs the given command with the arguments.
--   Depending on the exit code, either logs the stderr and exits fast or returns the stdout.
run :: (MonadIO m, MonadLogger m) => FilePath -> [String] -> m String
run cmd args = do
    (exitCode, stdout, stderr) <- liftIO $ System.readProcessWithExitCode cmd args ""

    if exitCode == ExitSuccess
        then return stdout
        else $(logError) (pack stderr) >> liftIO (exitWith 1)

