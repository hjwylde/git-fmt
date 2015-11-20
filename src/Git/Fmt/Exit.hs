
{-|
Module      : Git.Fmt.Exit
Description : Extra exit utilities.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Extra exit utilities.
-}

{-# LANGUAGE TemplateHaskell #-}

module Git.Fmt.Exit (
    -- * Exiting
    panicWith, panic, exitFast,
) where

import Control.Monad.IO.Class
import Control.Monad.Logger

import Data.Text (pack)

import System.Exit

-- | Panics, logging the error to stderr and exiting fast with the code.
panicWith :: (MonadIO m, MonadLogger m) => String -> Int -> m a
panicWith error code = $(logError) (pack error) >> exitFast code

-- | Panics, logging the error to stderr and exiting fast with 128.
panic :: (MonadIO m, MonadLogger m) => String -> m a
panic error = panicWith error 128

-- | Exits fast with the given code (may be 0 for success!).
exitFast :: (MonadIO m) => Int -> m a
exitFast 0 = liftIO exitSuccess
exitFast code = liftIO $ exitWith (ExitFailure code)

