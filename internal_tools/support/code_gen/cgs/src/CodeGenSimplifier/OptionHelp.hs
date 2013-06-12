module CodeGenSimplifier.OptionHelp (
  OptionHelp(..)
, optionHelp
, formatText
, formatOption
) where

import Data.List (inits, tails)

--------------------------------------------------------------------------------
data OptionHelp = OptionHelp {
    optCol :: Int
  , descCol :: Int
  , maxCol :: Int
  }

optionHelp :: OptionHelp
optionHelp = OptionHelp { optCol = 2, descCol = 30, maxCol = 80 }

msgWidth :: OptionHelp -> Int
msgWidth o = maxCol o - descCol o

--------------------------------------------------------------------------------

formatText :: OptionHelp -> String -> String
formatText o msg = removeTrailingNewLines $ unlines desc
  where
    indent = replicate (descCol o) ' '
    desc = map (indent ++) $ scrunch o msg

formatOption :: OptionHelp -> Maybe Char -> Maybe String -> String -> String
formatOption o short long msg = removeTrailingNewLines $ opt ++ unlines desc
  where
    indent = replicate (descCol o) ' '
    opt = replicate (optCol o) ' ' ++ optShort ++ optLong
    optShort = case short of
      Nothing -> "  "
      Just c -> '-' : [c]
    optLong = case long of
      Nothing -> ""
      Just str -> case short of
        Nothing -> "  --" ++ str
        Just _ ->  ", --" ++ str
    desc = mapHead f $ mapTail (indent ++) $ scrunch o msg
    f = if length opt >= descCol o
      then \line -> '\n' : indent ++ line
      else (drop (length opt) indent ++)

removeTrailingNewLines :: String -> String
removeTrailingNewLines = reverse . dropWhile (`elem` "\r\n") . reverse

mapHead :: (a -> a) -> [a] -> [a]
mapHead f (x:xs) = f x : xs
mapHead _ [] = []

mapTail :: (a -> a) -> [a] -> [a]
mapTail f (x:xs) = x : map f xs
mapTail _ [] = []

initsTails :: [a] -> [([a], [a])]
initsTails xs = zip (inits xs) (tails xs)

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (x, y) = (f x, y)

scrunch :: OptionHelp -> String -> [String]
scrunch o = scrunch' o . words

scrunch' :: OptionHelp -> [String] -> [String]
scrunch' _ [] = []
scrunch' o (w:ws)
  | length w > msgWidth o = w : scrunch' o ws
  | otherwise = case scrunch'' o (w:ws) of
    (line, []) -> [line]
    (line, rest) -> line : scrunch' o rest

scrunch'' :: OptionHelp -> [String] -> (String, [String])
scrunch'' o = pickBest . map (mapFst unwords) . initsTails
  where
    pickBest [] = error "Text.OptionHelp: TODO"
    pickBest xs = case takeWhile ((<= msgWidth o) . length . fst) xs of
      [] -> head xs
      ys -> last ys


