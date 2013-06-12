module Data.List.Extra (
  revInits
, mapHead
, stripSuffix
, droppingWhile
, spanN
) where

import Data.List (stripPrefix)

--------------------------------------------------------------------------------

revInits :: [a] -> [[a]]
revInits = scanl (flip (:)) []

mapHead :: (a -> a) -> [a] -> [a]
mapHead _ [] = []
mapHead f (x:xs) = f x : xs

stripSuffix :: Eq a => [a] -> [a] -> Maybe [a]
stripSuffix suffix = fmap reverse
  . stripPrefix (reverse suffix)
  . reverse

droppingWhile :: ([a] -> Bool) -> [a] -> [a]
droppingWhile _ [] = []
droppingWhile p xs@(_:xs')
  | p xs = droppingWhile p xs'
  | otherwise = xs

spanN :: Int -> (a -> Bool) -> [a] -> ([a], [a])
spanN n p xs
  | n <= 0 = ([], xs)
  | otherwise = let
      (ys, zs) = span p xs
      y = take 1 zs
      zs' = drop 1 zs
      (ys', ws) = spanN (n-1) p zs'
      in (ys ++ y ++ ys', ws)


