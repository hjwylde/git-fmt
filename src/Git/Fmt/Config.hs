
{-|
Module      : Git.Fmt.Config
Description : Configuration data structures.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Configuration data structures.
-}

{-# LANGUAGE OverloadedStrings #-}

module Git.Fmt.Config (
    -- * Config
    Config(..),
    emptyConfig,

    -- * Program
    Program(..),
    emptyProgram, programFor, unsafeProgramFor, supported,

    -- * Helper functions
    fileName,
) where

import Data.Aeson.Types
import Data.HashMap.Lazy    (toList)
import Data.List            (find)
import Data.Maybe           (fromJust, isJust)
import Data.Text            (Text)

-- | A list of programs.
data Config = Config {
    programs :: [Program]
    }
    deriving (Eq, Show)

instance FromJSON Config where
    parseJSON (Object obj)  = Config <$> mapM (\(key, value) ->
            parseJSON value >>= \program -> return program { name = key }
            ) (toList obj)
    parseJSON value         = typeMismatch "Config" value

-- | The empty config (no programs).
emptyConfig :: Config
emptyConfig = Config []

-- | A program has a semantic name, associated extensions and command.
--   The command string may contain variables to be replaced by surrounding them with '{{..}}'.
data Program = Program {
    name        :: Text,
    extensions  :: [Text],
    command     :: Text
    }
    deriving (Eq, Show)

instance FromJSON Program where
    parseJSON (Object obj)  = Program "" <$> obj .: "extensions" <*> obj .: "command"
    parseJSON value         = typeMismatch "Program" value

-- | The empty program (the command fails).
emptyProgram :: Program
emptyProgram = Program "" [] "false"

-- | Attempts to find a program for the given extension.
programFor :: Config -> Text -> Maybe Program
programFor config ext = find (\program -> ext `elem` extensions program) (programs config)

-- | Finds a program for the given extension or errors.
unsafeProgramFor :: Config -> Text -> Program
unsafeProgramFor config = fromJust . programFor config

-- | Checks if the given extension is supported (e.g., there is a program for it).
supported :: Config -> Text -> Bool
supported config = isJust . programFor config

-- | The file name of the default config file.
fileName :: String
fileName = ".omnifmt.yaml"

