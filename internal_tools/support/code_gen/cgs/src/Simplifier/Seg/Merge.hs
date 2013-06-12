{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Simplifier.Seg.Merge (
  mergeSegments
) where

import Control.Monad.State.Strict (State, runState, modify, gets)
import qualified Hoops.HC as HC
import qualified Hoops.HC.View as V
import Hoops.Key (Key)
import Hoops.SegPath (SegPath, isAnonymous)
import Token (Token)

--------------------------------------------------------------------------------

mergeSegments :: [Token] -> [Token]
mergeSegments = uncurry (flip seq) . flip runState st . mergeSegs
  where
    st = MergeState { openStack = [] }

data Open
  = Path SegPath
  | ByKey Key
  deriving (Show, Eq, Ord)

newtype MergeState = MergeState {
    openStack :: [Open]
  }

type Merger = State MergeState

infixr 5 <:>
(<:>) :: Functor f => a -> f [a] -> f [a]
(<:>) x = fmap (x :)

infixr 5 <++>
(<++>) :: Functor f => [a] -> f [a] -> f [a]
(<++>) xs = fmap (xs ++)

mergeSegs :: [Token] -> Merger [Token]
mergeSegs (V.openSegment -> Just (key, mKey, ts)) = handleOpen key mKey ts
mergeSegs (V.openSegmentByKey -> Just (key, ts)) = handleOpenByKey key ts
mergeSegs (V.closeSegment -> Just ts) = handleClose ts
mergeSegs (t : ts) = t <:> mergeSegs ts
mergeSegs [] = return []

handleOpen :: SegPath -> Maybe Key -> [Token] -> Merger [Token]
handleOpen path mKey ts = do
  modify $ \st -> st { openStack = Path path : openStack st }
  HC.openSegment path mKey <++> mergeSegs ts

handleOpenByKey :: Key -> [Token] -> Merger [Token]
handleOpenByKey key ts = do
  modify $ \st -> st { openStack = ByKey key : openStack st }
  HC.openSegmentByKey key <++> mergeSegs ts

class Openable a where
  mkOpen :: a -> Open
  hcOpen :: a -> [Token]

instance Openable Key where
  mkOpen = ByKey
  hcOpen = HC.openSegmentByKey

instance Openable (SegPath, Maybe Key) where
  mkOpen = Path . fst
  hcOpen = uncurry HC.openSegment

guaranteedSame :: Open -> Open -> Bool
guaranteedSame (Path p1) (Path p2) = p1 == p2 && not (isAnonymous p1)
guaranteedSame o1 o2 = o1 == o2

cannotCloseMsg :: String
cannotCloseMsg = "Cannot close segment with no open segments."

handleClose :: [Token] -> Merger [Token]
handleClose (V.openSegment -> Just (path, mKey, ts)) = handleClose' (path, mKey) ts
handleClose (V.openSegmentByKey -> Just (key, ts)) = handleClose' key ts
handleClose ts = HC.closeSegment <++> do
  modify $ \st -> case openStack st of
    _ : os -> st { openStack = os }
    [] -> err cannotCloseMsg
  mergeSegs ts
  where
    err msg = error $ "Merge.handleClose: " ++ msg

handleClose' :: Openable o => o -> [Token] -> Merger [Token]
handleClose' openable ts = do
  os <- gets openStack
  case os of
    o : os' -> let
      o' = mkOpen openable
      in if guaranteedSame o o'
        then mergeSegs ts
        else do
          modify $ \st -> st { openStack = o' : os' }
          HC.closeSegment <++> hcOpen openable <++> mergeSegs ts
    [] -> err cannotCloseMsg
  where
    err msg = error $ "Merge.handleClose': " ++ msg


