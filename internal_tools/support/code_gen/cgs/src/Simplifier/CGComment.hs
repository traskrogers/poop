{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Simplifier.CGComment (
  removeCGComments
) where

import Hoops.Quote (hc)
import Token (Token)
import qualified Token.String as TS

--------------------------------------------------------------------------------

removeCGComments :: [Token] -> [Token]
removeCGComments (viewCGComment -> Just ts) = removeCGComments ts
removeCGComments (t:ts) = t : removeCGComments ts
removeCGComments [] = []

viewCGComment :: [Token] -> Maybe [Token]
viewCGComment [hc| HC_Define_System_Options(@s); | s ts |] = if poorMan'sComment s
  then Just ts
  else Nothing
viewCGComment _ = Nothing

poorMan'sComment :: TS.StringType -> Bool
poorMan'sComment s = "code generation comment" `TS.isPrefixOf` s'
  && TS.count '=' s' <= 1
  && TS.count ',' s' == 0
  where
    s' = TS.unwords $ TS.words s


