
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
    emptyConfig, readConfig, nearestConfigFile, programFor, unsafeProgramFor, supported,

    -- * Program
    Program(..),
    emptyProgram, substitute, usesInputVariable, usesOutputVariable, inputVariableName,
    outputVariableName,

    -- * Helper functions
    defaultFileName,
) where

import Control.Arrow        (second)
import Control.Monad.Except
import Control.Monad.Extra
import Control.Monad.Logger

import Data.Aeson.Types
import Data.HashMap.Lazy (toList)
import Data.List         (find)
import Data.Maybe        (fromJust, isJust)
import Data.Text         (Text, cons, isInfixOf, pack, replace, snoc)
import Data.Yaml         (prettyPrintParseException)
import Data.Yaml.Include (decodeFileEither)

import System.Directory.Extra
import System.FilePath

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

-- | Reads the given config if possible.
readConfig :: (MonadIO m, MonadLogger m) => FilePath -> m (Maybe Config)
readConfig filePath = liftIO (decodeFileEither filePath) >>= \ethr -> case ethr of
    Left error      -> do
        logDebugN . pack $ filePath ++ ": error\n" ++ prettyPrintParseException error
        return Nothing
    Right config    -> return $ Just config

-- | Finds the nearest config file by searching from the given directory upwards.
nearestConfigFile :: MonadIO m => FilePath -> m (Maybe FilePath)
nearestConfigFile dir = findM (liftIO . doesFileExist) $ map (</> defaultFileName) parents
    where
        parents = takeWhile (\dir -> dir /= takeDrive dir) (iterate takeDirectory dir)

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
--   Arguments given are quoted and have any backslashes and double quotaiton marks escaped.
substitute :: Text -> [(Text, Text)] -> Text
substitute = foldr (uncurry replace . second (quote . escape))
    where
        quote   = cons '"' . (`snoc` '"')
        escape  = replace (pack "\"") (pack "\\\"") . replace (pack "\\") (pack "\\\\")

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

