
{-|
Module      : Git.Fmt.Command
Description : Options and handler for the git-fmt command.

Copyright   : (c) Henry J. Wylde, 2015
License     : BSD3
Maintainer  : public@hjwylde.com

Options and handler for the git-fmt command.
-}

module Git.Fmt.Command (
    -- * Options
    Options(..),

    -- * Handle
    handle,
) where


-- | Options.
data Options = Options {}
    deriving (Eq, Show)

-- | Builds the files according to the options.
handle :: Options -> IO ()
handle _ = return ()

