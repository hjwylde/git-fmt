
{-|
Module      : Main

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_HADDOCK hide, prune #-}

{-# LANGUAGE OverloadedStrings #-}

module Main (
    main,
) where

import Control.Monad
import Control.Monad.Logger

import              Data.ByteString.Char8   (ByteString)
import qualified    Data.ByteString.Char8   as BS
import              Data.List.Extra         (dropEnd, lower)
import              Data.Time               (getZonedTime, formatTime, defaultTimeLocale)

import Git.Fmt
import Git.Fmt.Options.Applicative.Parser

import Options.Applicative

import Prelude hiding (filter, log)

import System.IO
import System.Log.FastLogger


main :: IO ()
main = customExecParser gitFmtPrefs gitFmtInfo >>= \options ->
    runLoggingT (filter options (handle options)) (if optChatty options == Verbose then verboseLog else log)

filter :: Options -> LoggingT m a -> LoggingT m a
filter options = filterLogger (\_ level -> level >= minLevel)
    where
        minLevel = case optChatty options of
            Quiet   -> LevelError
            Default -> LevelInfo
            Verbose -> LevelDebug

log :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
log _ _ level msg = BS.hPutStrLn h (fromLogStr msg)
    where
        h = if level == LevelError then stderr else stdout

verboseLog :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
verboseLog _ _ level msg = do
    timestamp <- formatTime defaultTimeLocale "%F %T.%q" <$> getZonedTime

    forM_ (BS.lines $ fromLogStr msg) $ \line ->
        BS.hPutStrLn h (BS.unwords [BS.pack $ "[" ++ dropEnd 6 timestamp ++ "]", formatLevel level, line])
    where
        h = if level == LevelError then stderr else stdout

formatLevel :: LogLevel -> ByteString
formatLevel = BS.take 6 . BS.drop 5 . (`BS.append` "  ") . BS.pack . lower . show

