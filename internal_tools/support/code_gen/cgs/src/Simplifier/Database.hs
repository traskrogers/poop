{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Simplifier.Database (
  removeDatabaseControl
) where

import Hoops.Quote (hc)
import Token (Token)
import qualified Token.String as TS

--------------------------------------------------------------------------------

removeDatabaseControl :: [Token] -> [Token]
removeDatabaseControl (viewDatabaseControl -> Just ts) = removeDatabaseControl ts
removeDatabaseControl (t:ts) = t : removeDatabaseControl ts
removeDatabaseControl [] = []

viewDatabaseControl :: [Token] -> Maybe [Token]
viewDatabaseControl [hc| HC_Control_Update_By_Key(@k),@s); | _ s ts |] = if "database" `TS.isInfixOf` s
  then Just ts
  else Nothing
viewDatabaseControl _ = Nothing


