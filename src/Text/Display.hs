{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Text.Display
  ( mkDisplayText
  , mkDisplayTextFromStr
  , unDisplayText
  , dPrint

  -- * Re-exports
  , ppShow
  , pPrint

  -- * Types
  , Display(..)
  , DisplayText
  )
  where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO

import Text.Show.Pretty as Pretty

newtype DisplayText = DisplayText { _fromDisplayText :: Text } deriving (Eq)

instance Show DisplayText where
  show displayText = Text.unpack (unDisplayText displayText)

class Display a where
  display :: a -> DisplayText

instance {-# Overlappable #-} (Show a) => Display a where
  display a = (DisplayText . Text.pack . Pretty.ppShow) a

mkDisplayText :: Text -> DisplayText
mkDisplayText a = DisplayText a

unDisplayText :: DisplayText -> Text
unDisplayText a = _fromDisplayText a

mkDisplayTextFromStr :: String -> DisplayText
mkDisplayTextFromStr a = mkDisplayText (Text.pack a)

dShow :: Display a => a -> Text
dShow a = unDisplayText (display a)

dPrint :: Display a => a -> IO ()
dPrint a = TextIO.putStrLn (dShow a)
