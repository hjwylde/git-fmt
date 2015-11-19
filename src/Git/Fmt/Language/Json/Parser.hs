
{-|
Module      : Git.Fmt.Language.Json.Parser
Description : Parser for the JSON language.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Parser for the JSON language.
-}

module Git.Fmt.Language.Json.Parser (
    -- * Parser
    topLevelValue,
) where

import Text.JSON.Parsec
import Text.JSON.Types

-- | Parser for a top level JSON value (either an array or object).
topLevelValue :: Parser JSValue
topLevelValue = spaces >> topLevelValue'
    where
        topLevelValue' = choice [JSArray <$> p_array, JSObject <$> p_js_object] <?> "top level JSON value"

