
{-|
Module      : Git.Fmt.Language
Description : Utilities for working with a general language.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Utilities for working with a general language.
-}

module Git.Fmt.Language (
    -- * Languages
    Language(..),
    languages, extensions, supportedExtensions, parser, renderWithTabs,
) where

import Data.List (intercalate)

import Git.Fmt.Language.Json.Parser as Json
import Git.Fmt.Language.Json.Pretty ()

import Text.Parsec.String
import Text.PrettyPrint.HughesPJClass


-- | Supported languages.
data Language = Json
    deriving (Eq, Show)


-- | Array of supported languages.
languages :: [Language]
languages = [Json]

-- | Gets the known extensions for a language.
extensions :: Language -> [String]
extensions Json = map ('.':) ["json"]

-- | List of supported extensions.
supportedExtensions :: [String]
supportedExtensions = concatMap extensions languages

-- | Gets the parser for a language.
parser :: Language -> Parser Doc
parser Json = pPrint <$> Json.topLevelValue

-- | Renders the document using the default "style" and replaces any prefixed spaces with tabs.
renderWithTabs :: Doc -> String
renderWithTabs doc = intercalate "\n" $ map withTabs (lines $ render doc)
    where
        withTabs (' ':xs) = '\t':withTabs xs
        withTabs line = line

