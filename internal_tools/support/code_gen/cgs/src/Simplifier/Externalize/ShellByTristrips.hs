{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Simplifier.Externalize.ShellByTristrips (
  func
, writer
) where

import Data.List (intercalate)
import Data.Point (Point(..))
import Hoops.Key (Key)
import Simplifier.Externalize.View (stripSimpleDecls, getPoints, getInts, getDefineKey)
import Token (Token)

--------------------------------------------------------------------------------

func :: String
func = "HC_Insert_Shell_By_Tristrips"

writer :: FilePath -> [Token] -> IO (Maybe Key)
writer path ts = do
  writeFile path $ toHmf points tristrips faces
  return mKey
  where
    (points, tristrips, faces, mKey) = getData ts

--------------------------------------------------------------------------------

getData :: [Token] -> ([Point], [Integer], [Integer], Maybe Key)
getData (stripSimpleDecls -> 
  (getPoints "points" -> 
    (points, getInts "list" -> 
      (tristrips, getInts "list2" -> 
        (faces, getDefineKey ->
          (mKey)))))) = (reverse points, reverse tristrips, reverse faces, mKey)

toHmf :: [Point] -> [Integer] -> [Integer] -> String
toHmf ps ts fs = "(Shell_By_Tristrips\n\t(" ++ pstr ++ ")\n\t(" ++ tstr ++ ")\n\t(" ++ fstr ++ "))"
  where
    pstr = intercalate "" $ map showPoint ps
    tstr = unwords $ map show ts
    fstr = unwords $ map show fs

showPoint :: Point -> String
showPoint (Point x y z) = "(" ++ unwords (map show [x, y, z]) ++ ")"


