
{-|
Module      : Omnifmt.Exit
Description : Extra exit utilities.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Extra exit utilities.
-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Omnifmt.Exit (
    -- * Exiting
    panic, panic_, runPanic,
) where

import Control.Monad.Except
import Control.Monad.Logger

import Data.Text (pack)

import System.Exit

-- | Panics, logging the error to stderr and exiting fast with 128.
panic :: (MonadError ExitCode m, MonadIO m, MonadLogger m) => String -> m a
panic error = logErrorN (pack error) >> throwError (ExitFailure 128)

-- | Panics, logging the error to stderr and exiting fast with 128.
--   Rather than exiting fast using a 'MonadError', this method uses 'exitWith'
--   (@panic_ = runPanic . panic@).
panic_ :: (MonadIO m, MonadLogger m) => String -> m a
panic_ = runPanic . panic

-- | Runs the panic, calling 'exitWith' if the 'ExceptT' had an error thrown.
runPanic :: MonadIO m => ExceptT ExitCode m a -> m a
runPanic = runExceptT >=> either (liftIO . exitWith) return

