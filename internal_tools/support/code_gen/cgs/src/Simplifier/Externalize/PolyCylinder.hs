module Simplifier.Externalize.PolyCylinder (
  externalizePolyCylinder
) where

import C.Show (CShow(..))
import Control.Exception (assert)
import Data.FilePrototype (nextFile, FilePrototype)
import Data.List.Split (splitEvery)
import Hoops.Key (Key(..))
import Matcher
import Matcher.Block (leafBlock)
import Simplifier.Externalize (gReadMetafile, extractDefineKey)
import Token (Token, GenToken(..), isNumber, isString)
import Token.Number (Number(TInteger))

--------------------------------------------------------------------------------

externalizePolyCylinder :: FilePrototype -> [Token] -> IO [Token]
externalizePolyCylinder = ext

ext :: FilePrototype -> [Token] -> IO [Token]
ext fp ts = fmap (before ++) $ case mRes of
  Just (cData, after) -> do
    let (path, fp') = nextFile fp
    key <- writePolyCylinder path cData
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
    p = (== TIdentifier "HC_Insert_PolyCylinder")

toHmf :: Int -> Int -> [Token] -> Token -> String
toHmf ptCnt rCnt nums cap = "(PolyCylinder\n\t" ++ p pts ++ "\n\t" ++ p rs ++ "\n\t" ++ cshow cap ++ ")"
  where
    p s = '(' : s ++ ")"
    --
    pts = concatMap (p . unwords) $ splitEvery 3 $ map cshow $ take (3 * ptCnt) nums
    rs = unwords $ map cshow $ take rCnt $ drop (3 * ptCnt) nums

writePolyCylinder :: FilePath -> [Token] -> IO Key
writePolyCylinder path ts = do
  writeFile path $ toHmf ptCnt rCnt nums cap
  return key
  where
    cap = head $ filter isString ts
    nums = every2 $ drop 2 $ filter isNumber ts
    [ptCnt, rCnt] = map toInt $ take 2 $ filter isNumber ts
    key = extractDefineKey ts
    toInt (TNumber (TInteger n)) = fromInteger n
    toInt _ = assert False $ error "PolyCylinder.writePolyCylinder.toInt"

every2 :: [a] -> [a]
every2 [] = []
every2 (_:x:xs) = x : every2 xs
every2 _ = assert False $ error "PolyCylinder.every2"






