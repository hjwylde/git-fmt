
{-|
Module      : Git.Fmt.Options
Description : Optparse utilities.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Optparse utilities.
-}

module Git.Fmt.Options (
    -- * Options
    Options(..), Chatty(..), Mode(..), Files(..),

    -- * Optparse
    gitFmtPrefs, gitFmtInfo, gitFmt,
) where

import Data.Char    (isDigit)
import Data.Version (showVersion)

import Git.Fmt.Version as This

import Omnifmt.Config            as Config
import Options.Applicative
import Options.Applicative.Types (readerAsk)

-- | Options.
data Options = Options {
        optChatty    :: Chatty,
        optNull      :: Bool,
        optMode      :: Mode,
        optOperateOn :: Files,
        optThreads   :: Maybe Int,
        argPaths     :: [FilePath]
    }
    deriving (Eq, Show)

-- | Chattyness level.
data Chatty = Default | Quiet | Verbose
    deriving (Eq, Show)

-- | Run mode.
data Mode = Normal | DryRun | Diff
    deriving (Eq, Show)

-- | Operation files.
data Files = Tracked | Reference String
    deriving (Eq, Show)

-- | The default preferences.
--   Limits the help output to 100 columns.
gitFmtPrefs :: ParserPrefs
gitFmtPrefs = prefs $ columns 100

-- | An optparse parser of a git-fmt command.
gitFmtInfo :: ParserInfo Options
gitFmtInfo = info (infoOptions <*> gitFmt) (fullDesc <> progDesc')
    where
        infoOptions     = helper <*> version <*> numericVersion
        helper          = abortOption ShowHelpText $ mconcat [
            short 'h', hidden,
            help "Show this help text"
            ]
        version         = infoOption ("Version " ++ showVersion This.version) $ mconcat [
            long "version", short 'V', hidden,
            help "Show this binary's version"
            ]
        numericVersion  = infoOption (showVersion This.version) $ mconcat [
            long "numeric-version", hidden,
            help "Show this binary's version (without the prefix)"
            ]

        progDesc' = progDesc $ "Formats the paths given by applying the rules found in the root `" ++ Config.defaultFileName ++ "'"

-- | An options parser.
gitFmt :: Parser Options
gitFmt = Options
    <$> (
            flag' Quiet (mconcat [
            long "quiet", short 'q', hidden,
            help "Be quiet"
            ])
        <|> flag Default Verbose (mconcat [
            long "verbose", short 'v', hidden,
            help "Be verbose"
            ])
        )
    <*> switch (mconcat [
        long "null", short '0',
        help "Input files are delimited by a null terminator instead of white space"
        ])
    <*> modeOption (mconcat [
        long "mode", short 'm', metavar "MODE",
        value Normal, showDefaultWith $ const "normal",
        completeWith ["normal", "dry-run", "diff"],
        help "Specify the mode as either `normal', `dry-run' or `diff'"
        ])
    <*> (
            flag' Tracked (mconcat [
            long "operate-on-tracked",
            help "Operate on all tracked files (i.e., `git ls-files')"
            ])
        <|> Reference <$> strOption (mconcat [
            long "operate-on", metavar "REF",
            value "head", showDefault,
            help "Operate on all files in the reference (i.e., `git diff REF --name-only')"
            ])
        )
    <*> natOption (mconcat [
        long "threads", metavar "INT",
        value Nothing, showDefaultWith $ const "number of processors",
        help "Specify the number of threads to use"
        ])
    <*> many (strArgument $ mconcat [
        metavar "-- PATHS...",
        action "file"
        ])
    where
        modeOption  = option $ readerAsk >>= \opt -> case opt of
            "normal"    -> return Normal
            "dry-run"   -> return DryRun
            "diff"      -> return Diff
            _           -> readerError $ "unrecognised mode `" ++ opt ++ "'"
        natOption   = option $ readerAsk >>= \opt -> if all isDigit opt
            then return $ Just (read opt :: Int)
            else readerError $ "not a natural number `" ++ opt ++ "'"

