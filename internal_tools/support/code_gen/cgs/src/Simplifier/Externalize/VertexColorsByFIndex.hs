{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Simplifier.Externalize.VertexColorsByFIndex (
  func
, reader
, writer
) where

import Hoops.Key (Key, asLookup)
import Hoops.Quote (hc)
import Simplifier.Externalize.View (stripSimpleDecls, getFloats)
import Token (Token, GenToken(..))
import qualified Token.String as TS

--------------------------------------------------------------------------------

func :: String
func = "HC_MSet_Vertex_Colors_By_FIndex"

reader :: Maybe Key -> FilePath -> [Token]
reader (Just key) path = [
    TIdentifier "G_Read_Vertex_Colors_By_FIndex"
  , TSymbol "("
  ] ++ asLookup key ++ [
    TSymbol ","
  , TString $ TS.pack path
  , TSymbol ")"
  , TSymbol ";"
  ]
reader _ _ = error "VertexColorsByFIndex.reader: Must be passed a `Just key'"

writer :: FilePath -> [Token] -> IO (Maybe Key)
writer path ts = do
  writeFile path $ toFile geomType offset fidxs
  return $ Just key
  where
    (key, geomType, offset, fidxs) = getData ts

--------------------------------------------------------------------------------

getData :: [Token] -> (Key, TS.StringType, Integer, [Double])
getData (stripSimpleDecls -> 
  (getFloats "findices" ->
    (fidxs, [hc| HC_MSet_Vertex_Colors_By_FIndex(@k,@s,@d | key geomType offset _ |]))) 
      = (key, geomType, offset, reverse fidxs)
getData _ = error "VertexColorsByFIndex.getData: Malformed HC_MSet_Vertex_Colors_By_FIndex"

toFile :: TS.StringType -> Integer -> [Double] -> String
toFile geomType offset fidxs = unlines [
    TS.unpack geomType
  , show offset
  , show count
  , unlines $ map show fidxs
  ]
  where
    count = length fidxs


