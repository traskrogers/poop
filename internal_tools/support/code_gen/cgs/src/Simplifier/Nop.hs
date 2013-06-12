{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Simplifier.Nop (
  removeNopOpenCloses
) where

import Control.Monad ((<=<))
import Control.Monad.State.Strict (State, runState, modify)
import qualified Hoops.HC as HC
import qualified Hoops.HC.View as V
import Hoops.Quote (hc)
import Simplifier.Keys.Define (removeExtraneousDefines)
import Token (Token, GenToken(..))

--------------------------------------------------------------------------------

removeNopOpenCloses :: [Token] -> [Token]
removeNopOpenCloses = iterateToFixed remover

type Changer = State ChangeState

newtype ChangeState = ChangeState {
    changeHappened :: Bool
  }

iterateToFixed :: (a -> Changer a) -> a -> a
iterateToFixed f x = if changeHappened st'
  then iterateToFixed f x'
  else x'
  where
    (x', st') = runState (f x) st
    st = ChangeState { changeHappened = False }

remover :: [Token] -> Changer [Token]
remover = return
  <=< (return . removeExtraneousDefines)
  <=< changeAllWith stripNopCreate
  <=< (return . openCloseToCreate)
  <=< changeAllWith stripNopOpenClose
  <=< changeAllWith stripEmptyBlock

changeAllWith :: ([a] -> Maybe [a]) -> [a] -> Changer [a]
changeAllWith f (f -> Just xs) = do
  modify $ \st -> st { changeHappened = True }
  changeAllWith f xs
changeAllWith f (x:xs) = fmap (x :) $ changeAllWith f xs
changeAllWith _ [] = return []

stripEmptyBlock :: [Token] -> Maybe [Token]
-- The ; and { are in there so only simple empty blocks are stripped (ie: not `while(exp){/*do nothing*/}`)
stripEmptyBlock [hc| ;{} | ts|] = Just $ TSymbol ";" : ts
stripEmptyBlock [hc| {{} | ts|] = Just $ TSymbol "{" : ts
stripEmptyBlock _ = Nothing

openCloseToCreate :: [Token] -> [Token]
openCloseToCreate (V.openSegment -> Just (path, mKey, V.closeSegment -> Just ts))
    = HC.createSegment path mKey ++ openCloseToCreate ts
openCloseToCreate (t:ts) = t  : openCloseToCreate ts
openCloseToCreate [] = []

stripNopCreate :: [Token] -> Maybe [Token]
stripNopCreate (V.createSegment -> Just (_, Nothing, ts)) = Just ts
stripNopCreate _ = Nothing

stripNopOpenClose :: [Token] -> Maybe [Token]
stripNopOpenClose (V.open -> Just (func, V.close func -> Just ts)) = Just ts
stripNopOpenClose _ = Nothing


