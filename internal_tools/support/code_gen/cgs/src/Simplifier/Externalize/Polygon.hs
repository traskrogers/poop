{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Simplifier.Externalize.Polygon (
  func
, writer
) where

import Data.List (intercalate)
import Data.Point (Point(..))
import Hoops.Key (Key)
import Simplifier.Externalize.View (stripSimpleDecls, getPoints, getDefineKey)
import Token (Token)

--------------------------------------------------------------------------------

func :: String
func = "HC_Insert_Polygon"

writer :: FilePath -> [Token] -> IO (Maybe Key)
writer path ts = do
  writeFile path $ toHmf points
  return mKey
  where
    (points, mKey) = getData ts

--------------------------------------------------------------------------------

getData :: [Token] -> ([Point], Maybe Key)
getData (stripSimpleDecls -> 
  (getPoints "points" -> 
    (points, getDefineKey ->
      (mKey)))) = (reverse points, mKey)

toHmf :: [Point] -> String
toHmf ps = "(Polygon(" ++ pstr ++ "))"
  where
    pstr = intercalate "" $ map showPoint ps

showPoint :: Point -> String
showPoint (Point x y z) = "(" ++ unwords (map show [x, y, z]) ++ ")"


