{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Token.GenToken (
  GenToken(..)
, symbols
, isBool
, isChar
, isComment
, isDirective
, isIdentifier
, isKeyword
, isNumber
, isString
, isSymbol
) where

import C.Show (CShow(..))
import Control.DeepSeq (NFData(..), deepseq)
import Control.Exception (assert)
import Data.Binary
import Data.Char (ord, intToDigit)
import Data.List (intercalate)
import Language.Haskell.TH.Syntax (Lift(..))
import Numeric (showIntAtBase)
import Token.Directive (Directive)
import Token.Keyword (Keyword)
import Token.Number (Number)

--------------------------------------------------------------------------------

data GenToken s
  = TBool Bool
  | TChar Char
  | TComment s
  | TDirective (Directive s)
  | TIdentifier s
  | TKeyword Keyword
  | TNumber Number
  | TString s
  | TSymbol s
  deriving (Show, Read, Eq, Ord)

instance Lift s => Lift (GenToken s) where
  lift tok = case tok of
    TBool b -> [|TBool b|]
    TChar c -> [|TChar c|]
    TComment s -> [|TComment s|]
    TDirective d -> [|TDirective d|]
    TIdentifier s -> [|TIdentifier s|]
    TKeyword k -> [|TKeyword k|]
    TNumber n -> [|TNumber n|]
    TString s -> [|TString s|]
    TSymbol s -> [|TSymbol s|]

instance Binary s => Binary (GenToken s) where
  put t = case t of
    TBool b -> putWord8 0 >> put b
    TChar c -> putWord8 1 >> put c
    TComment s -> putWord8 2 >> put s
    TDirective d -> putWord8 3 >> put d
    TIdentifier s -> putWord8 4 >> put s
    TKeyword k -> putWord8 5 >> put k
    TNumber n -> putWord8 6 >> put n
    TString s -> putWord8 7 >> put s
    TSymbol s -> putWord8 8 >> put s
  get = do
    t <- getWord8
    case t of
      0 -> fmap TBool get
      1 -> fmap TChar get
      2 -> fmap TComment get
      3 -> fmap TDirective get
      4 -> fmap TIdentifier get
      5 -> fmap TKeyword get
      6 -> fmap TNumber get
      7 -> fmap TString get
      8 -> fmap TSymbol get
      _ -> assert False $ error "GenToken.get{Binary s => Token s}"

instance NFData s => NFData (GenToken s) where
  rnf t = case t of
    TBool b -> b `seq` ()
    TChar c -> c `seq` ()
    TComment s -> s `deepseq` ()
    TDirective d -> d `deepseq` ()
    TIdentifier s -> s `deepseq` ()
    TKeyword k -> k `seq` ()
    TNumber n -> n `deepseq` ()
    TString s -> s `deepseq` ()
    TSymbol s -> s `deepseq` ()

instance Functor GenToken where
  fmap f tok = case tok of
    TBool b -> TBool b
    TChar c -> TChar c
    TComment s -> TComment $ f s
    TDirective d -> TDirective $ fmap f d
    TIdentifier s -> TIdentifier $ f s
    TKeyword k -> TKeyword k
    TNumber n -> TNumber n
    TString s -> TString $ f s
    TSymbol s -> TSymbol $ f s

instance CShow s => CShow (GenToken s) where
  cshow t = case t of
    TBool b -> cshow b
    TChar c -> (\k -> '\'' : k ++ "'") $ case c of
      '"' -> "\""
      '?' -> "?"
      _ -> escape c
    TComment s -> "/*" ++ cshow s ++ "*/"
    TDirective d -> cshow d
    TIdentifier s -> cshow s
    TKeyword k -> cshow k
    TNumber n -> cshow n
    TString s -> let
      guts = unescapeSingleQuotes
        $ simplifyQuestionMarks
        $ intercalate ""
        $ map escape $ cshow s
      in '"' : guts ++ "\""
    TSymbol s -> cshow s

unescapeSingleQuotes :: String -> String
unescapeSingleQuotes s = case s of
  '\\':'\'':cs -> '\'' : unescapeSingleQuotes cs
  '\\':c:cs -> '\\' : c : unescapeSingleQuotes cs
  c:cs -> c : unescapeSingleQuotes cs
  "" -> ""

simplifyQuestionMarks :: String -> String
simplifyQuestionMarks = f
  where
    f ('\\':'?':'\\':'?':rest) = "\\?" ++ f ("\\?" ++ rest)
    f ('\\':'?':rest) = '?' : f rest
    f (c:rest) = c : f rest
    f "" = ""

hexify :: Int -> String
hexify n = showIntAtBase 16 intToDigit n ""

normalChar :: Char -> Bool
normalChar c = ' ' <= c && c <= '~'

escape :: Char -> String
escape c = case c of
  '\0' -> "\\0"
  '\a' -> "\\a"
  '\b' -> "\\b"
  '\f' -> "\\f"
  '\n' -> "\\n"
  '\r' -> "\\r"
  '\t' -> "\\t"
  '\v' -> "\\v"
  '\''-> "\\'"
  '?' -> "\\?"
  '"' -> "\\\""
  '\\'-> "\\\\"
  _ -> if normalChar c
    then [c]
    else "\\x" ++ hexify (ord c)

isBool :: GenToken s -> Bool
isBool (TBool{}) = True
isBool _ = False

isChar :: GenToken s -> Bool
isChar (TChar{}) = True
isChar _ = False

isComment :: GenToken s -> Bool
isComment (TComment{}) = True
isComment _ = False

isDirective :: GenToken s -> Bool
isDirective (TDirective{}) = True
isDirective _ = False

isIdentifier :: GenToken s -> Bool
isIdentifier (TIdentifier{}) = True
isIdentifier _ = False

isKeyword :: GenToken s -> Bool
isKeyword (TKeyword{}) = True
isKeyword _ = False

isNumber :: GenToken s -> Bool
isNumber (TNumber{}) = True
isNumber _ = False

isString :: GenToken s -> Bool
isString (TString{}) = True
isString _ = False

isSymbol :: GenToken s -> Bool
isSymbol (TSymbol{}) = True
isSymbol _ = False

symbols :: [String]
symbols = [
  "{", "}", "[", "]", "(", ")", "<", ">", "<=", ">=",
  "+", "-", "*", "/", "~", "!", "%", "^", "&", "|",
  "<<", ">>", "++", "--",
  "&&", "||", "==", "!=",
  ".", "->", ".*", "->*",
  "=", "+=", "-=", "*=", "/=", "%=", "<<=", ">>=", "&=", "^=", "|=",
  "?", ":", ",", ";", "::",
  "#", "##",
  "\\"
  ]








