{-# LANGUAGE OverloadedStrings #-}

module Token.ToCppCode (
  toCode
, toCodeAsIs
) where

import C.Show (cshow)
import Data.Depth (decOpenDepth, incOpenDepth, decBlockDepth, incBlockDepth, zeroDepth, getDepth, Depth)
import Data.List (isSuffixOf, isPrefixOf)
import Data.Tuple.Map (map1)
import Token (Token, GenToken(..))
import qualified Token.String as TS

--------------------------------------------------------------------------------

toCodeAsIs :: [Token] -> String
toCodeAsIs = fst . toCode'' (incBlockDepth zeroDepth)

toCode :: [[Token]] -> [String]
toCode = zipWith boilerPlate [0..] . toCode' d
  where
    d = incBlockDepth zeroDepth

boilerPlate :: Int -> String -> String
boilerPlate n code = before ++ code ++ after
  where
    before = unlines [
        "#include \"shared.h\""
      , ""
      , "int code_chain_" ++ show n ++ " (ENV_PARAM_TYPE env) {"
      ]
    after = unlines [
        "\treturn 0;"
      , "}"
      , ""
      ]

toCode' :: Depth -> [[Token]] -> [String]
toCode' _ [] = []
toCode' d (ts:tss) = s : toCode' d' tss
  where
    (s, d') = toCode'' d ts

toCode'' :: Depth -> [Token] -> (String, Depth)
toCode'' d = indent d . basicLayout

tabs :: Depth -> String -> String
tabs d = (replicate n '\t' ++)
  where
    n = getDepth d

indent :: Depth -> String -> (String, Depth)
indent d = map1 unlines . indent' d . filter (not . null) . lines

indent' :: Depth -> [String] -> ([String], Depth)
indent' d [] = ([], d)
indent' d (s : ss)
  | "{" `isSuffixOf` s = let d' = incBlockDepth d
      in map1 (tabs d s :) $ indent' d' ss
  | "}" `isSuffixOf` s = let d' = decBlockDepth d
      in map1 (tabs d' s :) $ indent' d' ss
  | "};" `isSuffixOf` s = let d' = decBlockDepth d
      in map1 (tabs d' s :) $ indent' d' ss
  | open s = let d' = incOpenDepth d
      in map1 (tabs d s :) $ indent' d' ss
  | close s = let d' = decOpenDepth d
      in map1 (tabs d' s :) $ indent' d' ss
  | otherwise = map1 (tabs d s :) $ indent' d ss
  where
    open str = any (`isPrefixOf` str) [
        "HC_Open"
      , "HC_KOpen"
      , "DEFINE(HC_Open"
      , "DEFINE(HC_KOpen"
      ]
    close = ("HC_Close" `isPrefixOf`)

doubleSpaced :: [TS.StringType]
doubleSpaced = TS.words "< > <= >= + - * / % ^ & | << >> && ||"
  ++ TS.words "== != = += -= *= /= %= <<= >>= &= ^= |= ? :"

basicLayout :: [Token] -> String
basicLayout tokens = case tokens of
  [] -> ""
  TSymbol ")" : TSymbol "{" : ts -> ") {\n" ++ basicLayout ts
  TSymbol "}" : TSymbol ";" : ts -> "\n\n\n};\n" ++ basicLayout ts
  TSymbol sym : ts -> case sym of
    ";" -> ";\n" ++ basicLayout ts
    "{" -> "{\n" ++ basicLayout ts
    "}" -> "\n}\n" ++ basicLayout ts
    "," -> ", " ++ basicLayout ts
    _ -> if sym `elem` doubleSpaced
      then ' ' : TS.unpack sym ++ " " ++ basicLayout ts
      else TS.unpack sym ++ basicLayout ts
  t : ts@(TDirective _ : _) -> cshow t ++ "\n" ++ basicLayout ts
  t@(TDirective _) : ts -> cshow t ++ "\n" ++ basicLayout ts
  b@(TBool _) : ts@(TSymbol _ : _) -> cshow b ++ basicLayout ts
  c@(TChar _) : ts@(TSymbol _ : _) -> cshow c ++ basicLayout ts
  i@(TIdentifier _) : ts@(TSymbol _ : _) -> cshow i ++ basicLayout ts
  k@(TKeyword _) : ts@(TSymbol _ : _) -> cshow k ++ basicLayout ts
  n@(TNumber _) : ts@(TSymbol _ : _) -> cshow n ++ basicLayout ts
  s@(TString _) : ts@(TSymbol _ : _) -> cshow s ++ basicLayout ts
  t : ts -> cshow t ++ " " ++ basicLayout ts










