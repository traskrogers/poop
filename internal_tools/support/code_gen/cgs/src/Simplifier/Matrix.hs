{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Simplifier.Matrix (
  shrinkMatrices
) where

import qualified C.Lexer.Compile.TH as L
import Hoops.Quote (hc)
import Data.List (intersperse)
import Token (Token, GenToken(..))
import Token.Number (Number)

--------------------------------------------------------------------------------

shrinkMatrices :: [Token] -> [Token]
shrinkMatrices (viewMatrix16 -> Just (m, ts)) = writeMatrix m ++ shrinkMatrices ts
shrinkMatrices (t:ts) = t : shrinkMatrices ts
shrinkMatrices [] = []

writeMatrix :: [Number] -> [Token]
writeMatrix m = $(L.compile "G_Write_Matrix(matrix,") ++ args ++ $(L.compile ");")
  where
    args = intersperse comma $ map TNumber m
    comma = TSymbol ","

viewMatrix :: [Token] -> Maybe (Number, [Token])
viewMatrix [hc| matrix[@i] = @n | _ n ts |] = Just (n, ts)
viewMatrix _ = Nothing

viewMatrix16 :: [Token] -> Maybe ([Number], [Token])
viewMatrix16 
  (viewMatrix -> Just (n1, 
  (viewMatrix -> Just (n2, 
  (viewMatrix -> Just (n3, 
  (viewMatrix -> Just (n4, 
  (viewMatrix -> Just (n5, 
  (viewMatrix -> Just (n6, 
  (viewMatrix -> Just (n7, 
  (viewMatrix -> Just (n8, 
  (viewMatrix -> Just (n9, 
  (viewMatrix -> Just (n10, 
  (viewMatrix -> Just (n11, 
  (viewMatrix -> Just (n12, 
  (viewMatrix -> Just (n13, 
  (viewMatrix -> Just (n14, 
  (viewMatrix -> Just (n15, 
  (viewMatrix -> Just (n16, ts
  ))))))))))))))))))))))))))))))))
  = Just ([n1, n2, n3, n4, n5, n6, n7, n8, n9, n10, n11, n12, n13, n14, n15, n16], ts)
viewMatrix16 _ = Nothing

