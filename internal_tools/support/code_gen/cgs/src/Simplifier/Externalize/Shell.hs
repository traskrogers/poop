{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Simplifier.Externalize.Shell (
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
func = "HC_Insert_Shell"

writer :: FilePath -> [Token] -> IO (Maybe Key)
writer path ts = do
  writeFile path $ toHmf points faces
  return mKey
  where
    (points, faces, mKey) = getData ts

--------------------------------------------------------------------------------

getData :: [Token] -> ([Point], [Integer], Maybe Key)
getData (stripSimpleDecls -> 
  (getPoints "points" -> 
    (points, getInts "list" -> 
      (faces, getDefineKey ->
        (mKey))))) = (reverse points, reverse faces, mKey)

toHmf :: [Point] -> [Integer] -> String
toHmf ps fs = "(Shell\n\t(" ++ pstr ++ ")\n\t(" ++ fstr ++ "))"
  where
    pstr = intercalate "" $ map showPoint ps
    fstr = unwords $ map show fs

showPoint :: Point -> String
showPoint (Point x y z) = "(" ++ unwords (map show [x, y, z]) ++ ")"


