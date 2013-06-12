module Data.Tuple.Map (
  map1
, map2
, map3
, map4
, lazyMapFst2
) where

import Data.Tuple.Select
import Data.Tuple.Update

--------------------------------------------------------------------------------

map1 :: (Upd1 a b c, Sel1 b d) => (d -> a) -> b -> c
map1 f x = upd1 (f $ sel1 x) x

map2 :: (Upd2 a b c, Sel2 b d) => (d -> a) -> b -> c
map2 f x = upd2 (f $ sel2 x) x

map3 :: (Upd3 a b c, Sel3 b d) => (d -> a) -> b -> c
map3 f x = upd3 (f $ sel3 x) x

map4 :: (Upd4 a b c, Sel4 b d) => (d -> a) -> b -> c
map4 f x = upd4 (f $ sel4 x) x

lazyMapFst2 :: (a -> c) -> (a, b) -> (c, b)
lazyMapFst2 f ~(x, y) = (f x, y)

