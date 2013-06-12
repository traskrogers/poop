module Simplifier.Externalize.Camera (
  externalizeCamera
) where

import C.Show (CShow(..))
import Data.FilePrototype (nextFile, FilePrototype)
import Matcher
import Matcher.Block (leafBlock)
import Simplifier.Externalize (hcReadMetafile)
import Token (Token, GenToken(..), isNumber, isString)

--------------------------------------------------------------------------------

externalizeCamera :: FilePrototype -> [Token] -> IO [Token]
externalizeCamera = ext

ext :: FilePrototype -> [Token] -> IO [Token]
ext fp ts = fmap (before ++) $ case mRes of
  Just (cData, after) -> do
    let (path, fp') = nextFile fp
    writeCamera path cData
    let meta = hcReadMetafile path
    fmap (meta ++) $ ext fp' after
  Nothing -> return []
  where
    maxTests = 30
    (before, mRes) = match pat ts
    pat = leafBlock (f . reverseInput)
    f rts = if any p $ take maxTests rts
      then Match
      else Fail
    p = (== TIdentifier "HC_Set_Camera")

toHmf :: [Token] -> Token -> String
toHmf nums proj = "(Camera\n" ++ unlines guts ++ ")"
  where
    guts = [p pos, p target, p up, w, h, cshow proj]
    p s = '(' : s ++ ")"
    --
    pos = unwords $ map cshow $ take 3 nums
    target = unwords $ map cshow $ take 3 $ drop 3 nums
    up = unwords $ map cshow $ take 3 $ drop 6 nums
    w = cshow $ nums !! 9
    h = cshow $ nums !! 10

writeCamera :: FilePath -> [Token] -> IO ()
writeCamera path ts = writeFile path $ toHmf nums proj
  where
    proj = head $ filter isString ts
    nums = filter isNumber ts


