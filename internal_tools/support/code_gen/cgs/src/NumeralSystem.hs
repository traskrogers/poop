module NumeralSystem (
  NumeralSystem(..)
, inSystem
, radix
, fromDigits
, fromDigits'
) where

import Data.Char (toLower)
import Data.List (foldl')
import Data.Maybe (fromJust)

--------------------------------------------------------------------------------

data NumeralSystem = Oct | Dec | Hex
  deriving (Show, Eq, Ord)

inSystem :: NumeralSystem -> Char -> Bool
inSystem sys c' = case sys of
  Oct -> ('0' <= c && c <= '7')
  Dec -> ('0' <= c && c <= '9')
  Hex -> ('0' <= c && c <= '9') || ('a' <= c && c <= 'f')
  where
    c = toLower c'

radix :: Num a => NumeralSystem -> a
radix Oct = 8
radix Dec = 10
radix Hex = 16

fromDigits :: (Num a, Ord a) => NumeralSystem -> [a] -> Maybe a
fromDigits sys = foldl' f $ Just 0
  where
    r = radix sys
    f mTot n = if n < r
      then fmap (\tot -> r * tot + n) mTot
      else Nothing

fromDigits' :: (Num a, Ord a) => NumeralSystem -> [a] -> a
fromDigits' sys = fromJust . fromDigits sys


