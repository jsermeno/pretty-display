{-# LANGUAGE CPP #-}
module Main where

import Numeric

import Text.Display
import Text.PrettyPrint.ANSI.Leijen as PP

data MyRecord = MyRecord
  { numerators :: [Double]
  , denominator :: Double
  } deriving (Show)

record :: MyRecord
record = MyRecord
  { numerators = [0, 5..100]
  , denominator = 1326
  }

instance
#if __GLASGOW_HASKELL >= 710
  {-# OVERLAPPING #-}
#endif
  Display MyRecord where
  display a = mkDisplayTextStr
      $ show
      $ toCol "MyRecord (percentage): " <> toCol (displayPerc a)
    where
      displayPerc a = showFFloat (Just 2) (toPerc a) "%"
      toPerc a = sum (numerators a) / denominator a * 100
      toCol a = (PP.dullgreen . PP.fill 25 . PP.text) a

main :: IO ()
main = do
  dPrint record
  pPrint record
