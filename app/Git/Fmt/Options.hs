
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
    Options(..), Chatty(..), Mode(..),

    -- * Optparse
    gitFmtPrefs, gitFmtInfo, gitFmt,
) where

import Data.Char    (isDigit)
import Data.Version (showVersion)

import Options.Applicative
import Options.Applicative.Types (readerAsk)

import Git.Fmt.Version as This

-- | Options.
data Options = Options {
        optChatty  :: Chatty,
        optNull    :: Bool,
        optMode    :: Mode,
        optThreads :: Maybe Int,
        argPaths   :: [FilePath]
    }
    deriving (Eq, Show)

-- | Chattyness level.
data Chatty = Default | Quiet | Verbose
    deriving (Eq, Show)

-- | Run mode.
data Mode = Normal | DryRun
    deriving (Eq, Show)

-- | The default preferences.
--   Limits the help output to 100 columns.
gitFmtPrefs :: ParserPrefs
gitFmtPrefs = prefs $ columns 100

-- | An optparse parser of a git-fmt command.
gitFmtInfo :: ParserInfo Options
gitFmtInfo = info (infoOptions <*> gitFmt) fullDesc
    where
        infoOptions = helper <*> version <*> numericVersion
        version = infoOption ("Version " ++ showVersion This.version) $ mconcat [
            long "version", short 'V', hidden,
            help "Show this binary's version"
            ]
        numericVersion = infoOption (showVersion This.version) $ mconcat [
            long "numeric-version", hidden,
            help "Show this binary's version (without the prefix)"
            ]

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
        help "Specify the mode as either `normal' or `dry-run'"
        ])
    <*> natOption (mconcat [
        long "threads", metavar "INT",
        value Nothing, showDefaultWith $ const "number of processors",
        help "Specify the number of threads to use"
        ])
    <*> many (strArgument $ mconcat [
        metavar "-- PATHS..."
        ])
    where
        natOption   = option $ readerAsk >>= \opt -> if all isDigit opt
            then return $ Just (read opt :: Int)
            else readerError $ "not a natural number `" ++ opt ++ "'"
        modeOption  = option $ readerAsk >>= \opt -> case opt of
            "normal"    -> return Normal
            "dry-run"   -> return DryRun
            _           -> readerError $ "unrecognised mode `" ++ opt ++ "'"

