
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
    pPrint (JSArray values)     = char '[' <> fsep (punctuate (char ',') (map pPrint values)) <> char ']'
    pPrint (JSBool bool)        = text $ lower (show bool)
    pPrint (JSNull)             = text "null"
    pPrint (JSObject obj)       = char '{' <> fsep (map (\(key, value) -> text key <> char ':' <+> pPrint value) (fromJSObject obj)) <> char '}'
    pPrint (JSRational _ rat)   = text $ show rat
    pPrint (JSString str)       = char '"' <> text (show str) <> char '"'

