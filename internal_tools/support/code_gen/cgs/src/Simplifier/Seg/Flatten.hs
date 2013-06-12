{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Simplifier.Seg.Flatten (
  flattenSegments
) where

import Control.Exception (assert)
import Control.Monad.State.Strict (State, runState, modify, gets)
import qualified Hoops.HC as HC
import qualified Hoops.HC.View as V
import Hoops.Key (Key)
import Hoops.SegPath (SegPath, isAbsolute)
import Token (Token)

--------------------------------------------------------------------------------

data Open
  = Path SegPath
  | ByKey Key

class Openable a where
  mkOpen :: a -> Open
  hcOpen :: a -> [Token]

instance Openable (SegPath, Maybe Key) where
  mkOpen = Path . fst
  hcOpen = uncurry HC.openSegment

instance Openable Key where
  mkOpen = ByKey
  hcOpen = HC.openSegmentByKey

instance Openable Open where
  mkOpen = id
  hcOpen (Path path) = HC.openSegment path Nothing
  hcOpen (ByKey key) = HC.openSegmentByKey key

data FlattenFlag
  = NoFlag
  | FlattenOccurred

newtype FlattenState = FlattenState {
    openStack :: [(Open, FlattenFlag)]
  }

type Flattener = State FlattenState

--------------------------------------------------------------------------------

infixr 5 <:>
(<:>) :: Functor f => a -> f [a] -> f [a]
(<:>) x = fmap (x :)

infixr 5 <++>
(<++>) :: Functor f => [a] -> f [a] -> f [a]
(<++>) xs = fmap (xs ++)

--------------------------------------------------------------------------------

flattenSegments :: [Token] -> [Token]
flattenSegments = uncurry (flip seq) . flip runState st . flatten
  where
    st = FlattenState { openStack = [] }

flatten :: [Token] -> Flattener [Token]
flatten (V.openSegment -> Just (path, mKey, ts)) = handleOpen (path, mKey) ts
flatten (V.openSegmentByKey -> Just (key, ts)) = handleOpen key ts
flatten (V.closeSegment -> Just ts) = handleClose ts
flatten (t : ts) = t <:> flatten  ts
flatten [] = return []

isRelative :: Open -> Bool
isRelative (ByKey _) = False
isRelative (Path path) = isAbsolute path /= Just True

handleOpen :: Openable o => o -> [Token] -> Flattener [Token]
handleOpen openable ts = do
  let o = mkOpen openable
  os <- gets openStack
  if null os || isRelative o
    then do
      modify $ \st -> st { openStack = (o, NoFlag) : os }
      hcOpen openable <++> flatten ts
    else do
      modify $ \st -> st { openStack = (o, FlattenOccurred) : os }
      HC.closeSegment <++> hcOpen openable <++> flatten ts

handleClose :: [Token] -> Flattener [Token]
handleClose ts = HC.closeSegment <++> do
  os <- gets openStack
  case os of
    (_, flag) : os' -> do
      modify $ \st -> st { openStack = os' }
      case flag of
        NoFlag -> flatten ts
        FlattenOccurred -> case map fst os' of
          --o : _ -> flatten (hcOpen o ++ ts)
          o : _ -> hcOpen o <++> flatten ts
          [] -> assert False $ error "Flatten.handleClose"
    [] -> error $ "Flatten.handleClose: Cannot close segment with no open segments."


