
{-|
Module      : System.IO.Extra'
Description : Extra extra IO utilities.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Extra extra IO utilities.
-}

{-# LANGUAGE TemplateHaskell #-}

module System.IO.Extra' (
    -- * Exiting
    panic, exitFast,
) where

import Control.Monad.IO.Class
import Control.Monad.Logger

import Data.Text (pack)

import System.Exit

-- | Panics, logging the error to stderr and exiting fast with 128.
panic :: (MonadIO m, MonadLogger m) => String -> m a
panic error = $(logError) (pack error) >> exitFast 128

-- | Exits fast with the given code (may be 0 for success!).
exitFast :: (MonadIO m) => Int -> m a
exitFast 0 = liftIO $ exitWith ExitSuccess
exitFast code = liftIO $ exitWith (ExitFailure code)

