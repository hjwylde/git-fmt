
{-|
Module      : Git.Fmt.Options.Applicative.Parser
Description : Optparse utilities for the git-fmt command.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Optparse utilities for the git-fmt command.
-}

module Git.Fmt.Options.Applicative.Parser (
    -- * Optparse for GitFmt
    gitFmtPrefs, gitFmtInfo, gitFmt,
) where

import Data.List    (nub)
import Data.Version (showVersion)

import Options.Applicative

import Git.Fmt
import Git.Fmt.Version as This


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
    <$> switch (mconcat [
        long "quiet", short 'q',
        help "Be quiet"
        ])
    <*> switch (mconcat [
        long "verbose", short 'v',
        help "Be verbose"
        ])
    <*> switch (mconcat [
        long "list-files", short 'l',
        help "List files and their state"
        ])
    <*> switch (mconcat [
        long "dry-run", short 'n',
        help "Don't perform any writes (useful with --list-files)"
        ])
    <*> fmap nub (many $ strArgument (mconcat [
        metavar "-- FILES..."
        ]))

