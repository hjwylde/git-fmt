
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

import Git.Fmt.Language
import Git.Fmt.Test

import System.Directory
import System.FilePath

import Test.Tasty


main :: IO ()
main = defaultMain =<< tests


tests :: IO TestTree
tests = do
    testsDir    <- getCurrentDirectory >>= \dir -> return $ dir </> "test" </> "json" </> "tests"
    testDirs    <- filter ((/= '.') . head) <$> getDirectoryContents testsDir
    testTrees   <- mapM (test Json . combine testsDir) testDirs

    return $ testGroup "JSON tests" testTrees

