{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ < 710
{-# LANGUAGE OverlappingInstances #-}
#endif
--------------------------------------------------------------------------------
-- |
-- Module     : Text.Display
-- Copyright  : (C) 2016 Justin Sermeno
-- License    : BSD3
-- Maintainer : Justin Sermeno
--
-- An instance @Display@ is similar to an instance of @Show@. The difference is that -- @Show@ is meant to return well-formed haskell expressions and a complementary
-- @Read@ instance should be possible. @Display@ is meant for human-readable output.
-- For instance, you could have a complex data structure output a chart by default
-- in GHCi, while still being able to derive an instance of @Show@ to inspect the
-- data structure when needed.
--------------------------------------------------------------------------------
module Text.Display
  ( -- * Types
    Display(..)
  , DisplayText

  -- * Converting to the 'DisplayText' type
  , mkDisplayText
  , unDisplayText
  , mkDt
  , unDt
  , mkDisplayTextStr
  , unDisplayTextStr
  , mkDtStr
  , unDtStr

  -- * Rendering the 'Display' class
  , dShow
  , dPrint

  -- * Re-exports
  , Pretty.ppShow
  , pPrint
  )
  where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

import qualified Text.Show.Pretty as Pretty

newtype DisplayText = DisplayText { _fromDisplayText :: Text } deriving (Eq)

instance Show DisplayText where
  show displayText = Text.unpack (unDisplayText displayText)

class Display a where
  display :: a -> DisplayText

instance
#if __GLASGOW_HASKELL__ >= 710
  {-# Overlappable #-}
#endif
  (Show a) => Display a where
  display a = (DisplayText . Text.pack . Pretty.ppShow) a

-- | Wrap 'Text' into a 'DisplayText'.
mkDisplayText :: Text -> DisplayText
mkDisplayText a = DisplayText a

-- | Alias for 'mkDt'
mkDt :: Text -> DisplayText
mkDt = mkDisplayText

-- | Unwrap 'DisplayText' to a 'Text'.
unDisplayText :: DisplayText -> Text
unDisplayText a = _fromDisplayText a

-- | Alias for 'unDisplayText'
unDt :: DisplayText -> Text
unDt = unDisplayText

-- | Wrap 'String' into a 'DisplayText'.
mkDisplayTextStr :: String -> DisplayText
mkDisplayTextStr a = mkDisplayText (Text.pack a)

-- | Alias for 'mkDisplayTextStr'
mkDtStr :: String -> DisplayText
mkDtStr = mkDisplayTextStr

-- | Unwrap 'DisplayText' to a 'String'.
unDisplayTextStr :: DisplayText -> String
unDisplayTextStr a = Text.unpack (unDisplayText a)

-- | Alias for 'unDisplayTextStr'
unDtStr :: DisplayText -> String
unDtStr = unDisplayTextStr

-- | Convert 'Display' instance into 'Text'.
dShow :: Display a => a -> Text
dShow a = Text.pack $ Pretty.ppShow $ display a

-- | Print 'Display' instance.
dPrint :: Display a => a -> IO ()
dPrint a = TextIO.putStrLn (dShow a)

-- | Pretty print 'Show' instance.
pPrint :: Show a => a -> IO ()
pPrint a = putStrLn (Pretty.ppShow a)
