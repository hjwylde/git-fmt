
{-|
Module      : Omnifmt.Config
Description : Configuration data structures.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Configuration data structures.
-}

{-# LANGUAGE OverloadedStrings #-}

module Omnifmt.Config (
    -- * Config
    Config(..),
    emptyConfig, readConfig, nearestConfigFile, defaultFileName, programFor, unsafeProgramFor,
    supported,

    -- * Program
    Program(..),
    emptyProgram, substitute, usesInputVariable, usesOutputVariable, inputVariableName,
    outputVariableName,
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

-- | A collection of 'Program's.
data Config = Config {
    programs :: [Program]
    }
    deriving (Eq, Show)

instance FromJSON Config where
    parseJSON (Object obj)  = Config <$> mapM (\(key, value) ->
            parseJSON value >>= \program -> return program { name = key }
            ) (toList obj)
    parseJSON value         = typeMismatch "Config" value

-- | An empty config (no programs).
emptyConfig :: Config
emptyConfig = Config []

-- | Reads a config from the given file path if possible.
--   If an error occurs it is logged using 'logDebugN'.
readConfig :: (MonadIO m, MonadLogger m) => FilePath -> m (Maybe Config)
readConfig filePath = liftIO (decodeFileEither filePath) >>= \ethr -> case ethr of
    Left error      -> do
        logDebugN . pack $ filePath ++ ": error\n" ++ prettyPrintParseException error
        return Nothing
    Right config    -> return $ Just config

-- | Finds the nearest config file by searching from the given directory upwards.
--
--   TODO (hjw): fix the bug where it won't search the root directory.
nearestConfigFile :: MonadIO m => FilePath -> m (Maybe FilePath)
nearestConfigFile dir = liftIO (canonicalizePath dir) >>= \dir' -> findM (liftIO . doesFileExist) $ map (</> defaultFileName) (parents dir')
    where
        parents dir' = takeWhile (\dir -> dir /= takeDrive dir) (iterate takeDirectory dir')

-- | The file name of the default config, '.omnifmt.yaml'.
defaultFileName :: FilePath
defaultFileName = ".omnifmt.yaml"

-- | Attempts to find a 'Program' for the given extension.
--   Programs are searched in order as provided by the 'Config' and the first match will be
--   returned.
programFor :: Config -> Text -> Maybe Program
programFor config ext = find (\program -> ext `elem` extensions program) (programs config)

-- | @fromJust . programFor@
unsafeProgramFor :: Config -> Text -> Program
unsafeProgramFor config = fromJust . programFor config

-- | Checks if the given extension is supported (i.e., there is a 'Program' for it).
supported :: Config -> Text -> Bool
supported config = isJust . programFor config

-- | A program has a semantic name, associated extensions and formatting command.
--   The command string may contain variables, denoted by strings surrounded with '{{..}}'.
--   The command should return a 0 exit code for success, or a non-0 exit code for failure.
data Program = Program {
    name       :: Text,     -- ^ A semantic name (has no impact on formatting).
    extensions :: [Text],   -- ^ A list of extensions, without a period prefix.
    command    :: Text      -- ^ A command to run in a shell that prettifies an input file and
                            --   writes to an output file.
    }
    deriving (Eq, Show)

instance FromJSON Program where
    parseJSON (Object obj)  = Program "" <$> obj .: "extensions" <*> obj .: "command"
    parseJSON value         = typeMismatch "Program" value

-- | The empty program (no extensions and the command always fails).
emptyProgram :: Program
emptyProgram = Program "" [] "false"

-- | Substitutes the mapping throughout the command.
--   The mapping is a tuple of @(variable, value)@.
--   Values given are quoted and have any backslashes and double quotaiton marks escaped.
substitute :: Text -> [(Text, Text)] -> Text
substitute = foldr (uncurry replace . second (quote . escape))
    where
        quote   = cons '"' . (`snoc` '"')
        escape  = replace (pack "\"") (pack "\\\"") . replace (pack "\\") (pack "\\\\")

-- | Checks whether the text uses the input variable ('inputVariableName').
usesInputVariable :: Text -> Bool
usesInputVariable = isInfixOf inputVariableName

-- | Checks whether the text uses the output variable ('outputVariableName').
usesOutputVariable :: Text -> Bool
usesOutputVariable = isInfixOf outputVariableName

-- | The input variable name, '{{input}}'.
inputVariableName :: Text
inputVariableName = "{{input}}"

-- | The output variable name, '{{output}}'.
outputVariableName :: Text
outputVariableName = "{{output}}"

