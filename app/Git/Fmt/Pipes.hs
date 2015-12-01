
{-|
Module      : Git.Fmt.Pipes
Description : Pipeline for formatting files.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Pipeline for formatting files.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections         #-}

module Git.Fmt.Pipes (
    -- * Transformers
    gitDiff,
) where

import Control.Monad.Except
import Control.Monad.Logger

import Omnifmt.Pipes
import Omnifmt.Process

import Pipes

-- | Prints out the git diff of all ugly files to standard output.
gitDiff :: (MonadIO m, MonadLogger m) => Pipe (Status, FilePath, FilePath) (Status, FilePath, FilePath) m ()
gitDiff = select [Ugly] $ \item@(_, uglyFilePath, prettyFilePath) -> do
    (_, stdout, _) <- runProcess "git" ["diff", "--no-index", "--", uglyFilePath, prettyFilePath]

    liftIO $ putStr stdout

    return item

