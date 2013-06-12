{-# LANGUAGE TypeSynonymInstances #-}

module Token.String (
  StringType
, module Data.ByteString.Char8
, GenParser
, Show(..)
, stripPrefix
, stripSuffix
) where

import Prelude hiding (Show, null, splitAt, length)
import qualified Prelude as P

import Data.ByteString.Char8
import Text.Parsec.ByteString (GenParser)

--------------------------------------------------------------------------------

type StringType = ByteString

--------------------------------------------------------------------------------

stripPrefix :: StringType -> StringType -> Maybe StringType
stripPrefix prefix str
  | null prefix = Just str
  | otherwise = case uncons prefix of
      Nothing -> Nothing
      Just (c, prefix') -> case uncons str of
        Nothing -> Nothing
        Just (k, str') -> if c == k
          then stripPrefix prefix' str'
          else Nothing

stripSuffix :: StringType -> StringType -> Maybe StringType
stripSuffix suffix str
  | suffix == end = Just begin
  | otherwise = Nothing
  where
    (begin, end) = splitAt (strN - suffN) str
    suffN = length suffix
    strN = length str

--------------------------------------------------------------------------------

class Show a where
  show :: a -> StringType


instance Show Char where
  show = pack . P.show

instance Show String where
  show = pack . P.show

instance Show StringType where
  show = id

instance Show Int where
  show = pack . P.show

instance Show Integer where
  show = pack . P.show

instance Show Double where
  show = pack . P.show


