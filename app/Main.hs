
{-|
Module      : Main

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_HADDOCK hide, prune #-}

module Main (
    main,
) where

import Control.Monad.Logger

import Git.Fmt
import Git.Fmt.Options.Applicative.Parser

import Options.Applicative

import Prelude hiding (filter, log)

import System.IO
import System.Log.FastLogger


main :: IO ()
main = customExecParser gitFmtPrefs gitFmtInfo >>= \options ->
    runLoggingT (filter options (handle options)) log

filter :: Options -> LoggingT m a -> LoggingT m a
filter options
    | optQuiet options      = filterLogger (\_ level -> level >= LevelError)
    | optVerbose options    = filterLogger (\_ level -> level >= LevelDebug)
    | optListUgly options   = filterLogger (\_ level -> level >= LevelInfo)
    | otherwise             = filterLogger (\_ level -> level >= LevelWarn)

-- TODO (hjw): find out why there are extra quote marks here
log :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
log _ _ LevelError msg  = putStrLn (init . drop 1 $ show (fromLogStr msg))
log _ _ _ msg           = hPutStrLn stderr (init . drop 1 $ show (fromLogStr msg))

