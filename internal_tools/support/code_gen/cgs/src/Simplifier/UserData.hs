{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Simplifier.UserData (
  removeUserData
) where

import Control.Monad.State.Strict (put, when)
import Hoops.HC.View (args, leafBlock)
import Hoops.Quote (hc)
import Token (Token, GenToken(..))
import qualified Token.String as TS

--------------------------------------------------------------------------------

removeUserData :: [Token] -> [Token]
removeUserData (stripUserData -> Just ts) = removeUserData ts
removeUserData (t:ts) = t : removeUserData ts
removeUserData [] = []

stripUserData :: [Token] -> Maybe [Token]
stripUserData [hc| HC_Set_User_Index(@[args]); | ts |] = Just ts
stripUserData (viewUserBlock -> Just (_, True, ts)) = Just ts
stripUserData _ = Nothing

viewUserBlock :: [Token] -> Maybe ([Token], Bool, [Token])
viewUserBlock = leafBlock False $ \t -> case t of
  TIdentifier name -> when (name `elem` hcUserFuncs) (put True)
  _ -> return ()

hcUserFuncs :: [TS.StringType]
hcUserFuncs = [
    "HC_Set_User_Options"
  , "HC_Set_Unicode_Options"
  , "HC_Set_User_Data"
  , "HC_Set_User_Data_By_Key"
  ]


