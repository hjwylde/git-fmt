
{-|
Module      : Main

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_HADDOCK hide, prune #-}

module Main (
    main,
) where

import Git.Fmt.Command
import Git.Fmt.Options.Applicative.Parser

import Options.Applicative


main :: IO ()
main = customExecParser gitFmtPrefs gitFmtInfo >>= handle

