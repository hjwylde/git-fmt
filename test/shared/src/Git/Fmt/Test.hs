
{-|
Module      : Git.Fmt.Test

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com
-}

{-# OPTIONS_HADDOCK hide, prune #-}

module Git.Fmt.Test (
    tests,
) where

import Control.Exception

import Data.ByteString.Lazy.Char8   (ByteString, pack)
import Data.List.Extra              (lower)

import Git.Fmt.Language

import System.Directory
import System.FilePath

import Test.Tasty
import Test.Tasty.Golden
import Text.Parsec          hiding (lower)


tests :: Language -> IO TestTree
tests language = do
    testsDir    <- getCurrentDirectory >>= \dir -> return $ dir </> "test" </> language' </> "tests"
    testDirs    <- filter ((/= '.') . head) <$> getDirectoryContents testsDir
    testTrees   <- mapM (test language . combine testsDir) testDirs

    return $ testGroup (language' ++ "tests") testTrees
    where
        language' = lower $ show language


test :: Language -> String -> IO TestTree
test language dir = return $ goldenVsString (takeFileName dir)
    (dir </> "expected-output" <.> extension language)
    (withCurrentDirectory dir $ fmt language)

fmt :: Language -> IO ByteString
fmt language = do
    input <- readFile inputFileName

    return . pack $ case runParser (parser language) () inputFileName input of
        Left error  -> show error
        Right doc   -> renderWithTabs doc
    where
        inputFileName = "input" <.> extension language

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory dir action = bracket getCurrentDirectory setCurrentDirectory $ \_ -> setCurrentDirectory dir >> action

