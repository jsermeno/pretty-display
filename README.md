# pretty-display: typeclass for human-readable display

In Haskell the `Show` typeclass displays the structure of a value as valid haskell code. However, there are times when you want to provide a richer display for a value while still retaining the benefits of having derived `Show` instances. This can be especially useful when working interactively in ghci. `pretty-display` provides a tiny registered package with the `Display` typeclass for just this purpose.

## GHCi usage

To use `Display` instances as the default in ghci create a `.ghci` file with the following:

```
import Text.Display

:set -interactive-print=Text.Display.dPrint
:def pp (\_ -> return ":set -interactive-print=Text.Display.dPrint")
:def npp (\_ -> return ":set -interactive-print=print")
```

<img src="https://cloud.githubusercontent.com/assets/197051/20393285/4d04a098-aca9-11e6-85ea-a025c5e752f1.png" alt="ghci example" width="638" height="213">

## Typeclass usage

By default, all instances of `Show` are also instances of `Display`. To create a custom instance you will need to use the `OVERLAPPING` pragma, and define the `display` method of type `a -> DisplayText`. For example:

```
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

instance {-# OVERLAPPING #-} Display MyRecord where
  display a = mkDisplayTextFromStr
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
```

## Using `Show` when defaulting to `Display`

If you've set ghci to use `dPrint` by default you can still print `Show` instances for debugging. For normal printing use `print`. For convenience `pretty-display` re-exports `pPrint` from the `pretty-show` package for pretty printing `Show` instances.
