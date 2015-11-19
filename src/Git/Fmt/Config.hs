
{-|
Module      : Git.Fmt.Config
Description : Configuration data.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

System process utilities.
-}

{-# LANGUAGE OverloadedStrings #-}

module Git.Fmt.Config (
    -- * Config
    Config(..),
    emptyConfig,

    -- * Program
    Program(..),
    emptyProgram, programFor, supported,

    -- * Helper functions
    fileName,
) where

import Control.Monad

import Data.Maybe           (isJust)
import Data.List            (find)
import Data.HashMap.Lazy    (toList)
import Data.Text            (Text)
import Data.Yaml


data Config = Config {
    programs :: [Program]
    }
    deriving (Eq, Show)

instance FromJSON Config where
    parseJSON (Object obj)  = Config <$> mapM (\(key, value) ->
            parseJSON value >>= \program -> return program { name = key }
        ) (toList obj)
    parseJSON _             = mzero

emptyConfig :: Config
emptyConfig = Config []


data Program = Program {
    name        :: Text,
    extensions  :: [Text],
    command     :: Text
    }
    deriving (Eq, Show)

instance FromJSON Program where
    parseJSON (Object obj)  = Program "" <$> obj .: "extensions" <*> obj .: "command"
    parseJSON _             = mzero

emptyProgram :: Program
emptyProgram = Program "" [] "false"

programFor :: Config -> Text -> Maybe Program
programFor config ext = find (\program -> ext `elem` extensions program) (programs config)

supported :: Config -> Text -> Bool
supported config = isJust . programFor config


fileName :: String
fileName = ".omnifmt.yaml"

