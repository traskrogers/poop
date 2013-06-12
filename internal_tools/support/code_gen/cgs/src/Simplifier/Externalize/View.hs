{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Simplifier.Externalize.View (
  stripSimpleDecls
, getDefineKey
, getFloats
, getInts
, getPoints
) where

import Data.Point (Point(..))
import Hoops.HC.View (defSt, begDef, endDef, args)
import Hoops.Key (Key)
import Hoops.Quote (hc)
import Token (Token, GenToken(..))
import Token.Keyword (Keyword(..))
import qualified Token.String as TS

--------------------------------------------------------------------------------

iterateView0 :: ([a] -> Maybe [a]) -> [a] -> [a]
iterateView0 v (v -> Just xs) = iterateView0 v xs
iterateView0 _ xs = xs

iterateView1 :: ([a] -> Maybe (b, [a])) -> [a] -> ([b], [a])
iterateView1 v (v -> Just (y, xs)) = case iterateView1 v xs of
  ~(ys, xs') -> (y:ys, xs')
iterateView1 _ xs = ([], xs)

isTypeLike :: Keyword -> Bool
isTypeLike = (`elem` kws)
  where
    kws = [
        KAuto, KBool, KChar, KConst, KDouble, KFloat, KInt, KLong
      , KShort, KSigned, KUnsigned, KVoid, KVolatile, KWcharT
      ]


stripSimpleDecl :: [Token] -> Maybe [Token]
stripSimpleDecl ts = if isDecl
  then Just $ drop 1 $ dropWhile (/= TSymbol ";") rest
  else Nothing
  where
    (d, rest) = span (`notElem` [TSymbol ";", TSymbol "=", TSymbol ",]"]) ts
    isDecl = case d of
      TKeyword kw : _ -> isTypeLike kw
      TIdentifier _ : d' -> case d' of
        TIdentifier _ : _ -> True
        TKeyword kw : _ -> isTypeLike kw
        TSymbol sym : _ -> sym `elem` ["*", "&"]
        _ -> False
      _ -> False

stripSimpleDecls :: [Token] -> [Token]
stripSimpleDecls = iterateView0 stripSimpleDecl


getPoint :: TS.StringType -> [Token] -> Maybe (Point, [Token])
getPoint var [hc| @i[@d].x = @f; @i[@d].y = @f; @i[@d].z = @f; | i1 d1 x i2 d2 y i3 d3 z ts |]
  | any (/= var) [i1, i2, i3] = Nothing
  | d1 /= d2 || d2 /= d3 = Nothing
  | otherwise = Just (Point x y z, ts)
getPoint _ _ = Nothing

getPoints :: TS.StringType -> [Token] -> ([Point], [Token])
getPoints var = iterateView1 $ getPoint var

getFloat :: TS.StringType -> [Token] -> Maybe (Double, [Token])
getFloat var [hc| @i[@d] = @f; | i _ f ts |]
  | var == i = Just (f, ts)
  | otherwise = Nothing
getFloat _ _ = Nothing

getFloats :: TS.StringType -> [Token] -> ([Double], [Token])
getFloats var = iterateView1 $ getFloat var

getInt :: TS.StringType -> [Token] -> Maybe (Integer, [Token])
getInt var [hc| @i[@d] = @d; | i _ d ts |]
  | var == i = Just (d, ts)
  | otherwise = Nothing
getInt _ _ = Nothing

getInts :: TS.StringType -> [Token] -> ([Integer], [Token])
getInts var = iterateView1 $ getInt var

getDefineKey :: [Token] -> Maybe Key
getDefineKey [hc| :defSt: @{begDef}@i(@[args])@{endDef}; | _ mKey _ |] = mKey
getDefineKey (_:ts) = getDefineKey ts
getDefineKey _ = Nothing


