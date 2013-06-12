{-# LANGUAGE OverloadedStrings #-}

module Hoops.Key (
  Key
, key1
, key2
, keyReps
, asDefine
, asLookup
) where

import Token (Token, GenToken(..))
import Token.Number (Number(..))

--------------------------------------------------------------------------------

data Key
  = Key1 Integer
  | Key2 Integer Integer
  deriving (Eq, Ord)

instance Show Key where
  show (Key1 k) = "Key(" ++ show k ++ ")"
  show (Key2 k1 k2) = "Key(" ++ show k1 ++ "," ++ show k2 ++ ")"

key1 :: Integer -> Key
key1 = Key1

key2 :: Integer -> Integer -> Key
key2 = Key2

keyReps :: Integer -> [Key]
keyReps k = [key2 k extra, key1 k]
  where
    extra = if k < 0
      then -1
      else 0

--------------------------------------------------------------------------------

tokenize :: Key -> [Token]
tokenize (Key1 k) = [TNumber $ TInteger k]
tokenize (Key2 k1 k2) = [TNumber $ TInteger k1, TSymbol ",", TNumber $ TInteger k2]

asDefine :: Key -> [Token] -> [Token]
asDefine key toks = [
    TIdentifier "DEFINE"
  , TSymbol "("
  ] ++ toks ++ [
    TSymbol ","
  ] ++ tokenize key ++ [
    TSymbol ")"
  ]

asLookup :: Key -> [Token]
asLookup key = [
    TIdentifier "LOOKUP"
  , TSymbol "("
  ] ++ tokenize key ++ [
    TSymbol ")"
  ]


