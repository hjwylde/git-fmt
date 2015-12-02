
{-|
Module      : Main

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK hide, prune #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module Main (
    main,
) where

import           Control.Concurrent
import           Control.Monad.Catch    (MonadMask (..))
import           Control.Monad.Except
import           Control.Monad.Extra
import           Control.Monad.Logger
import           Control.Monad.Parallel (MonadParallel (..))
import qualified Control.Monad.Parallel as Parallel
import           Control.Monad.Reader

import           Data.List.Extra    (dropEnd, intersect, linesBy, lower)
import           Data.Maybe         (fromJust, fromMaybe, isNothing)
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO       as T
import           Data.Time          (defaultTimeLocale, formatTime, getZonedTime)
import           Data.Tuple.Extra   (fst3)

import Git.Fmt.Options

import Omnifmt.Config  as Config
import Omnifmt.Exit
import Omnifmt.Pipes
import Omnifmt.Process

import Options.Applicative

import           Pipes
import           Pipes.Concurrent
import qualified Pipes.Prelude    as Pipes
import           Prelude          hiding (filter, log)

import System.Directory.Extra
import System.Exit
import System.FilePath
import System.IO
import System.IO.Temp
import System.Log.FastLogger

instance MonadParallel m => MonadParallel (LoggingT m) where
    bindM2 f ma mb = LoggingT $ \g -> bindM2 (f' g) (runLoggingT ma g) (runLoggingT mb g)
        where
            f' g a b = runLoggingT (f a b) g

main :: IO ()
main = do
    options     <- customExecParser gitFmtPrefs gitFmtInfo
    let chatty  = if optMode options == Diff then Quiet else optChatty options

    flip runLoggingT (log $ optChatty options) . filter chatty $ do
        exitCode <- fst3 <$> runProcess "git" ["rev-parse", "--show-toplevel"]
        when (exitCode /= ExitSuccess) $ panic_ ".git/: not found"

        mFilePath <- Config.nearestConfigFile "."
        when (isNothing mFilePath) . panic_ $ Config.defaultFileName ++ ": not found"

        mConfig <- Config.readConfig $ fromJust mFilePath
        when (isNothing mConfig) . panic_ $ fromJust mFilePath ++ ": error"

        runReaderT (handle options) (fromJust mConfig)

handle :: (MonadIO m, MonadLogger m, MonadMask m, MonadParallel m, MonadReader Config m) => Options -> m ()
handle options = do
    filePaths <- runPanic $ if null (argPaths options)
        then initialFilePaths options
        else liftM2 intersect (initialFilePaths options) (providedFilePaths options)

    numThreads <- liftIO getNumCapabilities >>= \numCapabilities ->
        return $ fromMaybe numCapabilities (optThreads options)

    (output, input) <- liftIO $ spawn unbounded

    withSystemTempDirectory "git-fmt" $ \tmpDir -> do
        runEffect $ each (map (\filePath -> omnifmt filePath (tmpDir </> filePath)) filePaths) >-> toOutput output

        liftIO performGC

        Parallel.forM_ [1..numThreads] $ \_ ->
            runEffect (fromInput input >-> pipeline >-> runner (optMode options)) >>
            liftIO performGC

providedFilePaths :: MonadIO m => Options -> m [FilePath]
providedFilePaths options = concatMapM expandDirectory $ concatMap splitter (argPaths options)
    where
        splitter = if optNull options then linesBy (== '\0') else (:[])
        expandDirectory path = ifM (liftIO $ doesDirectoryExist path) (liftIO $ listFilesRecursive path) (return [path])

initialFilePaths :: (MonadError ExitCode m, MonadIO m, MonadLogger m) => Options -> m [FilePath]
initialFilePaths options = linesBy (== '\0') <$> case optOperateOn options of
    Tracked         -> runProcess_ "git" ["ls-files", "-z"]
    Reference ref   -> runProcess_ "git" ["diff", ref, "--name-only", "-z"]

pipeline :: (MonadIO m, MonadLogger m, MonadReader Config m) => Pipe (Status, FilePath, FilePath) (Status, FilePath, FilePath) m ()
pipeline = checkFileSupported >-> checkFileExists >-> createPrettyFile >-> runProgram >-> checkFilePretty
    where
        createPrettyFile = select [Unknown] $ \item@(_, _, prettyFilePath) -> do
            liftIO $ createDirectoryIfMissing True (takeDirectory prettyFilePath)

            return item

runner :: (MonadIO m, MonadLogger m) => Mode -> Consumer (Status, FilePath, FilePath) m ()
runner Normal   = commit    >-> printFileStatus logFunction >-> Pipes.drain
runner DryRun   = cat       >-> printFileStatus logFunction >-> Pipes.drain
runner Diff     = diff      >-> Pipes.drain

logFunction :: MonadLogger m => Status -> Text -> m ()
logFunction status
    | status `elem` [Unknown, Error, Timeout]       = logWarnN
    | status `elem` [Unsupported, NotFound, Pretty] = logDebugN
    | otherwise                                     = logInfoN

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

