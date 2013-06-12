module Data.WildString (
  WildString(..)
, (?=)
, (=?)
) where

import qualified Token.String as TS

--------------------------------------------------------------------------------

infixr 4 ?=, =?, *=

newtype WildString = Wild String

(=?) :: TS.StringType -> WildString -> Bool
(=?) = flip (?=)


(?=) :: WildString -> TS.StringType -> Bool
Wild "" ?= str = TS.null str
Wild (k:'?':ks) ?= str = case TS.uncons str of
  Nothing -> Wild ks ?= str
  Just (c, rest) -> (k *= c && (Wild ks ?= rest)) || (Wild ks ?= str)
w@(Wild (k:'*':ks)) ?= str = case TS.uncons str of
  Nothing -> Wild ks ?= str
  Just (c, rest) -> (Wild ks ?= str) || (k *= c && (w ?= rest))
Wild (k:ks) ?= str = case TS.uncons str of
  Just (c, rest) -> k *= c && (Wild ks ?= rest)
  Nothing -> False


(*=) :: Char -> Char -> Bool
'.' *= _ = True
'#' *= c = '0' <= c && c <= '9'
k *= c = k == c


