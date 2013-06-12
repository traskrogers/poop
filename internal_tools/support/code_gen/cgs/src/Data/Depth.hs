module Data.Depth (
  Depth
, zeroDepth
, getDepth
, incOpenDepth
, decOpenDepth
, incBlockDepth
, decBlockDepth
) where

import Control.Exception (assert)
import Data.List.Extra (mapHead)

--------------------------------------------------------------------------------

data Depth = Depth { openDepths :: [Int] }

zeroDepth :: Depth
zeroDepth = Depth { openDepths = [0] }

getDepth :: Depth -> Int
getDepth d = sum ods + length ods - 1
  where
    ods = openDepths d

valid :: Depth -> Bool
valid d = not (null ods) && all (>= 0) ods
  where
    ods = openDepths d

incOpenDepth :: Depth -> Depth
incOpenDepth d = assert (valid d) $ d { openDepths = mapHead (+1) ods }
  where
    ods = openDepths d

decOpenDepth :: Depth -> Depth
decOpenDepth d = assert (valid d) $ d { openDepths = mapHead f ods  }
  where
    ods = openDepths d
    f n = max 0 $ n - 1

incBlockDepth :: Depth -> Depth
incBlockDepth d = assert (valid d) $ d { openDepths = 0 : ods }
  where
    ods = openDepths d

decBlockDepth :: Depth -> Depth
decBlockDepth d = assert (valid d) $ assert (valid d') d'
  where
    ods = tail $ openDepths d
    d' = d { openDepths = ods }










