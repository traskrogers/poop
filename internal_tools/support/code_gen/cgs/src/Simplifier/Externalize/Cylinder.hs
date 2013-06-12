module Simplifier.Externalize.Cylinder (
  externalizeCylinder
) where

import C.Show (CShow(..))
import Data.FilePrototype (nextFile, FilePrototype)
import Hoops.Key (Key(..))
import Matcher
import Matcher.Block (leafBlock)
import Simplifier.Externalize (gReadMetafile, extractDefineKey)
import Token (Token, GenToken(..), isNumber, isString)

--------------------------------------------------------------------------------

externalizeCylinder :: FilePrototype -> [Token] -> IO [Token]
externalizeCylinder = ext

ext :: FilePrototype -> [Token] -> IO [Token]
ext fp ts = fmap (before ++) $ case mRes of
  Just (cData, after) -> do
    let (path, fp') = nextFile fp
    key <- writeCylinder path cData
    let meta = gReadMetafile key path
    fmap (meta ++) $ ext fp' after
  Nothing -> return []
  where
    maxTests = 30
    (before, mRes) = match pat ts
    pat = leafBlock (f . reverseInput)
    f rts = if any p $ take maxTests rts
      then Match
      else Fail
    p = (== TIdentifier "HC_Insert_Cylinder")

toHmf :: [Token] -> Token -> String
toHmf nums cap = "(Cylinder" ++ unwords guts ++ ")"
  where
    guts = [p first, p second, r, cshow cap]
    p s = '(' : s ++ ")"
    --
    first = unwords $ map cshow $ take 3 nums
    second = unwords $ map cshow $ take 3 $ drop 3 nums
    r = cshow $ nums !! 6

writeCylinder :: FilePath -> [Token] -> IO Key
writeCylinder path ts = do
  writeFile path $ toHmf nums cap
  return key
  where
    cap = head $ filter isString ts
    nums = filter isNumber ts
    key = extractDefineKey ts






