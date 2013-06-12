{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Simplifier.Keys (
  gatherUsedKeys
) where

import Control.Exception (assert)
import Data.Char (isHexDigit)
import Data.Set (Set)
import qualified Data.Set as Set
import Hoops.Key (Key, keyReps)
import Hoops.Quote (hc)
import Numeric (readHex)
import Token (Token)
import qualified Token.String as TS

--------------------------------------------------------------------------------

gatherUsedKeys :: [Token] -> Set Key
gatherUsedKeys = Set.fromList . gatherUsedKeys'

gatherUsedKeys' :: [Token] -> [Key]
gatherUsedKeys' (viewKeys -> Just (keys, ts)) = keys ++ gatherUsedKeys' ts
gatherUsedKeys' (_ : ts) = gatherUsedKeys' ts
gatherUsedKeys' [] = []

viewKeys :: [Token] -> Maybe ([Key], [Token])
viewKeys (viewLookup -> Just (key, ts)) = Just ([key], ts)
viewKeys (viewPathKeys -> Just res) = Just res
viewKeys _ = Nothing

viewLookup :: [Token] -> Maybe (Key, [Token])
viewLookup [hc| @k | key ts |] = Just (key, ts)
viewLookup _ = Nothing

viewPathKeys :: [Token] -> Maybe ([Key], [Token])
viewPathKeys [hc| @s | s ts |] = Just (keysInString s, ts)
viewPathKeys _ = Nothing

keysInString :: TS.StringType -> [Key]
keysInString s = case TS.uncons s of
  Just ('@', s') -> case parseHex s' of
    Just (num, s'') -> keyReps num ++ keysInString s''
    Nothing -> keysInString s'
  Just (_, s') -> keysInString s'
  Nothing -> []

parseHex :: TS.StringType -> Maybe (Integer, TS.StringType)
parseHex s = case (\(s1, s2) -> (TS.unpack s1, s2)) $ TS.span isHexDigit s of
  ([], _) -> Nothing
  (cs, rest) -> Just $ (readHex' cs, rest)

readHex' :: String -> Integer
readHex' s = case readHex s of
  [(num, _)] -> num
  _ -> assert False $ error "Hoops.Key: internal error"


