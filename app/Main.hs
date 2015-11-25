
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
import Control.Monad.Reader

import           Data.List.Extra    (dropEnd, lower)
import           Data.Maybe         (fromJust, isNothing)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO       as T
import           Data.Time          (defaultTimeLocale, formatTime, getZonedTime)

import Git.Fmt
import Git.Fmt.Config  as Config
import Git.Fmt.Exit
import Git.Fmt.Options

import Options.Applicative

import Prelude hiding (filter, log)

import System.IO
import System.Log.FastLogger

main :: IO ()
main = do
    options     <- customExecParser gitFmtPrefs gitFmtInfo
    let chatty  = optChatty options

    flip runLoggingT (log chatty) $ filter chatty $ do
        mConfigFilePath <- Config.nearestConfigFile "."
        when (isNothing mConfigFilePath) $ panic (Config.defaultFileName ++ ": not found")
        config <- Config.readConfig $ fromJust mConfigFilePath

        runReaderT (handle options) config

filter :: Chatty -> LoggingT m a -> LoggingT m a
filter Quiet    = filterLogger (\_ level -> level >= LevelError)
filter Default  = filterLogger (\_ level -> level >= LevelInfo)
filter Verbose  = filterLogger (\_ level -> level >= LevelDebug)

log :: Chatty -> Loc -> LogSource -> LogLevel -> LogStr -> IO ()
log chatty _ _ level msg = do
    timestamp <- formatTime defaultTimeLocale "%F %T.%q" <$> getZonedTime

    forM_ (T.lines . T.decodeUtf8 $ fromLogStr msg) $ \line ->
        T.hPutStrLn h $ if chatty == Verbose
            then T.unwords [T.pack $ "[" ++ dropEnd 6 timestamp ++ "]", formattedLevel, line]
            else line
    where
        h               = if level == LevelError then stderr else stdout
        formattedLevel  = T.justifyLeft 6 ' ' . T.drop 5 . T.pack . lower $ show level

