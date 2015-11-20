
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
    emptyConfig, programFor, unsafeProgramFor, supported,

    -- * Program
    Program(..),
    emptyProgram, substitute, usesInputVariable, usesOutputVariable, inputVariableName,
    outputVariableName,

    -- * Helper functions
    defaultFileName,
) where

import Data.Aeson.Types
import Data.HashMap.Lazy (toList)
import Data.List.Extra   (find)
import Data.Maybe        (fromJust, isJust)
import Data.Text         (Text, isInfixOf, replace)

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

-- | Attempts to find a program for the given extension.
programFor :: Config -> Text -> Maybe Program
programFor config ext = find (\program -> ext `elem` extensions program) (programs config)

-- | Finds a program for the given extension or errors.
unsafeProgramFor :: Config -> Text -> Program
unsafeProgramFor config = fromJust . programFor config

-- | Checks if the given extension is supported (e.g., there is a program for it).
supported :: Config -> Text -> Bool
supported config = isJust . programFor config

-- | A program has a semantic name, associated extensions and command.
--   The command string may contain variables to be replaced by surrounding them with '{{..}}'.
data Program = Program {
    name       :: Text,
    extensions :: [Text],
    command    :: Text
    }
    deriving (Eq, Show)

instance FromJSON Program where
    parseJSON (Object obj)  = Program "" <$> obj .: "extensions" <*> obj .: "command"
    parseJSON value         = typeMismatch "Program" value

-- | The empty program (the command fails).
emptyProgram :: Program
emptyProgram = Program "" [] "false"

-- | Substitutes the mapping throughout the command.
substitute :: Text -> [(Text, Text)] -> Text
substitute = foldr (uncurry replace)

-- | Checks whether the text uses the input variable.
usesInputVariable :: Text -> Bool
usesInputVariable = isInfixOf inputVariableName

-- | Checks whether the text uses the output variable.
usesOutputVariable :: Text -> Bool
usesOutputVariable = isInfixOf outputVariableName

-- | The input variable name.
inputVariableName :: Text
inputVariableName = "{{input}}"

-- | The output variable name.
outputVariableName :: Text
outputVariableName = "{{output}}"

-- | The file name of the default config.
defaultFileName :: FilePath
defaultFileName = ".omnifmt.yaml"

