
{-|
Module      : Git.Fmt.Language.Json.Pretty
Description : Pretty instances for the JSON language.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Pretty instances for the JSON language.
-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Git.Fmt.Language.Json.Pretty where

import Data.List.Extra (lower)

import Text.JSON.Types
import Text.PrettyPrint.HughesPJClass


instance Pretty JSValue where
    pPrint (JSArray values)     = cat [char '[', nest 1 (sep $ punctuate (char ',') (map pPrint values)), char ']']
    pPrint (JSBool bool)        = text $ lower (show bool)
    pPrint (JSNull)             = text "null"
    pPrint (JSObject obj)
        | null keyValues    = text "{}"
        | otherwise         = char '{' $+$ nest 1 (vcat $ punctuate (char ',') keyValueDocs) $+$ char '}'
        where
            keyValueDocs    = map (\(key, value) -> char '"' <> text key <> text "\":" <+> pPrint value) keyValues
            keyValues       = fromJSObject obj
    pPrint (JSRational _ rat)   = text $ show (fromRational rat :: Double)
    pPrint (JSString str)       = char '"' <> text (fromJSString str) <> char '"'

