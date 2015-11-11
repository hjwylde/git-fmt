
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
    extensions, parser,
) where

import Git.Fmt.Language.Json.Pretty ()

import Text.JSON.Parsec                 as Json
import Text.PrettyPrint.HughesPJClass


data Language = Json

extensions :: Language -> [String]
extensions Json = ["json"]

parser :: Language -> Parser Doc
parser Json = pPrint <$> Json.p_value

