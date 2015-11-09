
{-|
Module      : Git.Fmt.Command
Description : Options and handler for the git-fmt command.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Options and handler for the git-fmt command.
-}

module Git.Fmt.Command (
    -- * Options
    Options(..),

    -- * Handle
    handle,
) where

import Control.Monad.Reader

import Git.Libgit2
import Git.Repository

import System.Exit
import System.Process


-- | Options.
data Options = Options {}
    deriving (Eq, Show)

-- | Builds the files according to the options.
handle :: Options -> IO ()
handle _ = topLevelDir >>= \dir -> withRepository lgFactory dir $ do
    _ <- ask
    return ()

topLevelDir :: IO FilePath
topLevelDir = do
    (exitCode, stdout, _) <- readCreateProcessWithExitCode (proc "git" ["rev-parse", "--show-toplevel"]) ""

    when (exitCode /= ExitSuccess) $ ioError (userError "not a git repository")

    return $ init stdout

