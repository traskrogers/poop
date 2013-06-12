{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Hoops.Quote (
  hc
) where

import qualified C.Lexer.Compile as L
import Control.Monad.ListM
import Control.Monad.State.Strict
import Data.Char
import Data.List
import Data.List.Split
import Hoops.Key (Key, key1, key2)
import Language.Haskell.Meta.Parse (parseExp, parsePat)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax.LiftExtra ()
import Language.Haskell.TH.Quote
import Prelude hiding (error)
import qualified Prelude as P
import Token (Token, GenToken(..))
import Token.Number (Number(..))
import qualified Token.String as TS

--------------------------------------------------------------------------------

error :: String -> String -> a
error func msg = P.error $ func ++ ": " ++ msg

--------------------------------------------------------------------------------

-- More convenient tuple representation for manipulation.
-- Alternative would be to nest tuples (1, (2, (3, ..))).
-- Always construct with a () at the end of the tuple:
--    This is important for type level logic as well as for
--    strictness correctness.
infixr 9 :*
data a :* b = a :* !b
  deriving (Show, Eq, Ord)

class Flatten a b | a -> b where
  flatten :: a -> b

instance (Flatten a r) => Flatten (() :* a) r where
  flatten (_ :* rest) = flatten rest

instance ((a :* c) ~ r, Flatten b c) => Flatten (a :* b) r where
  flatten (x :* rest) = (x :* flatten rest)

instance (a ~ r) => Flatten a r where
  flatten x = x

--------------------------------------------------------------------------------

data Stripper a
  = Pred (a -> Bool)
  | UseMatcher

--------------------------------------------------------------------------------

boolToMaybe :: Bool -> Maybe ()
boolToMaybe True = Just ()
boolToMaybe False = Nothing

takeWhileKeepBalanced :: (Char -> Bool) -> String -> String
takeWhileKeepBalanced p = flip evalState [0,0,0] . takeWhileM pM
  where
    pM c = do
      ns@[n0, n1, n2] <- get
      let b = p c
      if all (== (0 :: Int)) ns && not b
        then return False
        else do
          case c of
            '(' -> modify $ const [n0+1, n1, n2]
            ')' -> modify $ const [n0-1, n1, n2]
            '[' -> modify $ const [n0, n1+1, n2]
            ']' -> modify $ const [n0, n1-1, n2]
            '{' -> modify $ const [n0, n1, n2+1]
            '}' -> modify $ const [n0, n1, n2-1]
            _ -> return ()
          return True

class Monad m => StripPrefix m i x o | i x -> o where
  stripPrefixBy :: i -> [Stripper x] -> [x] -> m (Maybe ([x] :* o))

instance Monad m => StripPrefix m () x () where
  stripPrefixBy _ [] xs = return $ Just (xs :* ())
  stripPrefixBy _ _ [] = return Nothing
  stripPrefixBy _ (Pred p:ps) (x:xs) = case p x of
    True -> stripPrefixBy () ps xs
    False -> return Nothing
  stripPrefixBy _ (UseMatcher : _) _ = return Nothing

instance (Monad m, i' ~ ([x] -> m (Maybe (o', [x]))), StripPrefix m i x o) => StripPrefix m (i' :* i) x (o' :* o) where
  stripPrefixBy fs (Pred p : ss) (x:xs) = case p x of
    True -> stripPrefixBy fs ss xs
    False -> return Nothing
  stripPrefixBy (f :* fs) (UseMatcher : ss) xs = do
    fxs <- f xs
    case fxs of
      Just (y, xs') -> do
        res <- stripPrefixBy fs ss xs'
        case res of
          Just (xs'' :* res') -> return $ Just (xs'' :* y  :* res')
          Nothing -> return Nothing
      Nothing -> return Nothing
  stripPrefixBy _ _ _ = return Nothing

bindingExists :: Name -> Q Bool
bindingExists name = recover (return False) (reify name >> return True)

hc :: QuasiQuoter
hc = QuasiQuoter undefined pat undefined undefined

stripFront :: String -> String
stripFront = dropWhile (== ' ')

stripEnd :: String -> String
stripEnd = reverse . stripFront . reverse

strip :: String -> String
strip = stripEnd . stripFront

splitFromEnd :: Eq a => a -> [a] -> ([a], [a])
splitFromEnd x xs = (reverse ys, reverse zs)
  where
    ys = maybe ys' id $ stripPrefix [x] ys'
    (zs, ys') = break (x ==) $ reverse xs

rotateR :: [a] -> [a]
rotateR [] = []
rotateR xs = last xs : init xs

pat :: String -> PatQ
pat input = viewP [| fmap flatten . $(mkMatcher body) |] $ conP 'Just $ [mkTup results]
  where
    results = rotateR $ words results'
    (body, results') = splitFromEnd '|' input
    mkTup [] = conP '() []
    mkTup (s:ss) = infixP (either (error "pat.mkTup" noParse) return $ parsePat s) '(:*) $ mkTup ss
      where
        noParse = "Could not parse pattern `" ++ s ++ "'"

mkMatcher :: String -> ExpQ
mkMatcher body = do
  preds <- mkPreds pattern
  [| $(eval) . $(varE 'stripPrefixBy `appE` captures `appE` (return $ ListE preds)) |]
  where
    (st, pattern) = case stripFront body of
      ':':cs -> let
        stStr = takeWhileKeepBalanced (/= ':') cs
        rest = case drop (length stStr) cs of
          ':' : rest' -> rest'
          _ -> error "mkMatcher" $ "No closing ':' for state parameter"
        noParse = "Could not parse state parameter `" ++ stStr ++ "'"
        in (either (error "mkMatcher" noParse) id $ parseExp stStr, rest)
      _ -> (ConE '(), body)
    (captures, numCaptures) = mkCaptures pattern
    eval = [| flip evalState $(return st) |]

mapTail :: (a -> a) -> [a] -> [a]
mapTail f (x:xs) = x : map f xs
mapTail _ [] = []

mapJust :: (a -> b) -> [Maybe a] -> [Maybe b]
mapJust f (Just x : ms) = Just (f x) : mapJust f ms
mapJust f (Nothing : ms) = Nothing : mapJust f ms
mapJust _ [] = []

data Escape a = Unescaped a | Escaped a
  deriving (Show, Eq)

sinkMatchers :: String -> [Maybe String]
sinkMatchers = id
  . intersperse Nothing
  . map Just
  . mapTail dropCapture
  . map (map unescape)
  . sepBy [Unescaped '@']
  . escape
  . (' ' :)

escape :: String -> [Escape Char]
escape "" = []
escape "\\" = error "escape" "Invalid escape"
escape ('\\':c:cs) = Escaped c : escape cs
escape (c:cs) = Unescaped c : escape cs

unescape :: Escape a -> a
unescape (Unescaped x) = x
unescape (Escaped x) = x

dropCapture :: String -> String
dropCapture [] = error "dropCapture" "Invalid capture syntax"
dropCapture (c:cs)
  | toLower c `elem` ['a'..'z'] = cs
  | c == '{' = drop 1 $ dropWhile (/= '}') cs
  | c == '[' = drop 1 $ dropWhile (/= ']') cs
  | otherwise = error "dropMatcher" "Invalid capture syntax"

moveMaybe :: [Maybe [a]] -> [[Maybe a]]
moveMaybe [] = []
moveMaybe (m:ms) = case m of
  Nothing -> [Nothing] : moveMaybe ms
  Just xs -> map Just xs : moveMaybe ms

mkPreds :: String -> Q [Exp]
mkPreds = sequence . map f . concat . moveMaybe . mapJust L.compile . sinkMatchers
  where
    f (Just x) = [| Pred (x ==) |]
    f Nothing = conE 'UseMatcher

mkCaptures :: String -> (ExpQ, Int)
mkCaptures "" = (conE '(), 0)
mkCaptures ('@':'{':cs) = let
  funcStr = takeWhileKeepBalanced (/= '}') cs
  noParse = "Could not parse capture `" ++ funcStr ++ "'"
  func = either (error "mkCaptures" noParse) id $ parseExp funcStr
  cs' = drop (length funcStr + 1) cs
  (caps, n) = mkCaptures cs'
  in (infixE (Just $ return func) (conE '(:*)) $ Just caps, n + 1)
mkCaptures ('@':'[':cs) = let
  funcStr = takeWhileKeepBalanced (/= ']') cs
  noParse = "Could not parse capture `" ++ funcStr ++ "'"
  func = either (error "mkCaptures" noParse) id $ parseExp
    $ "return . (" ++ funcStr ++ ")"
  cs' = drop (length funcStr + 1) cs
  (caps, n) = mkCaptures cs'
  in (infixE (Just $ return func) (conE '(:*)) $ Just caps, n + 1)
mkCaptures ('@':c:cs) = let
  noParse = "Could not parse capture `" ++ [c] ++ "'"
  lam = case c of
    'd' -> [| return . captureInteger |]
    'f' -> [| return . captureDouble |]
    'i' -> [| return . captureIdentifier |]
    'k' -> [| return . captureKey |]
    'n' -> [| return . captureNumber |]
    's' -> [| return . captureString |]
    _ -> error "mkMatchers" noParse
  (caps, n) = mkCaptures cs
  in (infixE (Just lam) (conE '(:*)) $ Just caps, n + 1)
mkCaptures ('@': rest) = error "mkMatchers" $ "Cannot parse capture " ++ take 1 rest
mkCaptures ('\\':_:cs) = mkCaptures cs
mkCaptures (_ :cs) = mkCaptures cs

--------------------------------------------------------------------------------

captureInteger :: [Token] -> Maybe (Integer, [Token])
captureInteger (captureNumber -> Just (TInteger n, ts)) = Just (n, ts)
captureInteger _ = Nothing

captureDouble :: [Token] -> Maybe (Double, [Token])
captureDouble (captureNumber -> Just (num, ts)) = case num of
  TInteger n -> Just (fromInteger n, ts)
  TDouble d -> Just (d, ts)
captureDouble _ = Nothing

captureIdentifier :: [Token] -> Maybe (TS.StringType, [Token])
captureIdentifier (TIdentifier i : ts) = Just (i, ts)
captureIdentifier _ = Nothing

captureKey :: [Token] -> Maybe (Key, [Token])
captureKey (TIdentifier "LOOKUP" : TSymbol "(" : ts) = case captureKey' ts of
  Just (key, TSymbol ")" : ts') -> Just (key, ts')
  Just _ -> Nothing
  Nothing -> Nothing
captureKey _ = Nothing

captureKey' :: [Token] -> Maybe (Key, [Token])
captureKey' ts = case ts of
  TNumber (TInteger k1) : TSymbol "," : TNumber (TInteger k2) : ts' -> Just (key2 k1 k2, ts')
  TNumber (TInteger k) : ts' -> Just (key1 k, ts')
  _ -> Nothing

captureNumber :: [Token] -> Maybe (Number, [Token])
captureNumber (TNumber n : ts) = Just (n, ts)
captureNumber _ = Nothing

captureString :: [Token] -> Maybe (TS.StringType, [Token])
captureString (TString s : ts) = Just (s, ts)
captureString _ = Nothing


