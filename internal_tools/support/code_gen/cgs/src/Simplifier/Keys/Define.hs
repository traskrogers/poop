{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Simplifier.Keys.Define (
  removeExtraneousDefines
) where

import Data.Set (Set)
import qualified Data.Set as Set
import Hoops.HC.View (arg)
import Hoops.Key (Key, key1, key2)
import Hoops.Quote (hc)
import Simplifier.Keys (gatherUsedKeys)
import Token (Token, GenToken(..))

--------------------------------------------------------------------------------

stripDefine :: Set Key -> [Token] -> Maybe [Token]
stripDefine usedKeys [hc| DEFINE(@[arg], | a ts |] = case stripDefine' ts of
  Just (key, TSymbol ")" : ts') ->  if key `Set.member` usedKeys
    then Nothing
    else Just $ a ++ ts'
  _ -> Nothing
stripDefine _ _ = Nothing

stripDefine' :: [Token] -> Maybe (Key, [Token])
stripDefine' [hc| @d,@d | n1 n2 ts |] = Just (key2 n1 n2, ts)
stripDefine' [hc| @d | n ts |] = Just (key1 n, ts)
stripDefine' _ = Nothing

removeExtraneousDefines :: [Token] -> [Token]
removeExtraneousDefines tokens = go tokens
  where
    usedKeys = gatherUsedKeys tokens
    go (stripDefine usedKeys -> Just ts) = go ts
    go (t:ts) = t : go ts
    go [] = []


