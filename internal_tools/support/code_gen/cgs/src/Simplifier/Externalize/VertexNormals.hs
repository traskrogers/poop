{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Simplifier.Externalize.VertexNormals (
  func
, reader
, writer
) where

import Data.Point (Point(..))
import Hoops.Key (Key, asLookup)
import Hoops.Quote (hc)
import Simplifier.Externalize.View (stripSimpleDecls, getPoints)
import Token (Token, GenToken(..))
import qualified Token.String as TS

--------------------------------------------------------------------------------

func :: String
func = "HC_MSet_Vertex_Normals"

reader :: Maybe Key -> FilePath -> [Token]
reader (Just key) path = [
    TIdentifier "G_Read_Vertex_Normals"
  , TSymbol "("
  ] ++ asLookup key ++ [
    TSymbol ","
  , TString $ TS.pack path
  , TSymbol ")"
  , TSymbol ";"
  ]
reader _ _ = error "VertexNormals.reader: Must be passed a `Just key'"

writer :: FilePath -> [Token] -> IO (Maybe Key)
writer path ts = do
  writeFile path $ toFile offset normals
  return $ Just key
  where
    (key, offset, normals) = getData ts

--------------------------------------------------------------------------------

getData :: [Token] -> (Key, Integer, [Point])
getData (stripSimpleDecls -> 
  (getPoints "normals" ->
    (normals, [hc| HC_MSet_Vertex_Normals(@k,@d | key offset _ |]))) = (key, offset, reverse normals)
getData _ = error "VertexNormals.getData: Malformed HC_MSet_Vertex_Normals"

showPoint :: Point -> String
showPoint (Point x y z) = unwords $ map show [x, y, z]

toFile :: Integer -> [Point] -> String
toFile offset normals = unlines [
    show offset
  , show count
  , unlines $ map showPoint normals
  ]
  where
    count = length normals



