{-# LANGUAGE OverloadedStrings #-}

module C.Lexer (
  runLexer
) where

import C.Show (cshow)
import Control.Applicative ((<$), Applicative(..))
import Control.Monad (MonadPlus(..))
import qualified Data.ByteString.Lazy.Char8 as LC
import Data.Char (toUpper, chr, ord, toLower, isUpper)
import Data.List (foldl')
import Data.ListTrie.Map (TrieMap)
import qualified Data.ListTrie.Map as Trie
import Data.Map (Map)
import Data.Monoid (Monoid(mappend), Last(..))
import Data.Represent (Representer, mkRepresenter, represent)
import Numeric (readFloat)
import NumeralSystem
import Text.Parsec
import Text.Parsec.ByteString.Lazy (GenParser)
import Token
import Token.Directive
import Token.Keyword
import Token.Number (Number(..))
import qualified Token.String as TS

--------------------------------------------------------------------------------

--testLexer :: String -> Either ParseError [Token]
--testLexer str = case runLexer Nothing Nothing . LC.pack $ str of
--  Left err -> Left err
--  Right (ts, _) -> Right ts

--------------------------------------------------------------------------------

newtype LexState = LexState {
    representer :: Representer Token
  }

type Lexer a = GenParser Char LexState a

intern :: Token -> Lexer Token
intern tok = do
  st <- getState
  let r = representer st
      (tok', r') = represent tok r
  putState st { representer = r' }
  return tok'

--------------------------------------------------------------------------------

pack :: Lexer String -> Lexer TS.StringType
pack = fmap TS.pack

eol :: Lexer ()
eol = do
  _ <- try (string "\n\r")
    <|> try (string "\r\n")
    <|> string "\n"
    <|> string "\r"
    <?> "newline"
  return ()

lineSpaces :: Lexer ()
lineSpaces = many (oneOf " \t") >> return ()

wholeWord :: Lexer a -> Lexer a
wholeWord p = do
  res <- p
  notFollowedBy (alphaNum <|> char '_')
  return res

atMost :: Int -> Lexer a -> Lexer [a]
atMost n
  | n < 0 = error "C.Lexer.atMost needs a non-negative number"
  | otherwise = atMost' n

atMost' :: Int -> Lexer a -> Lexer [a]
atMost' 0 _ = return []
atMost' n lexer = go <|> return []
  where
    go = lexer >>= \res -> fmap (res :) $ atMost' (n-1) lexer

first :: [Lexer a] -> Lexer a
first = foldr1 (<|>)

--------------------------------------------------------------------------------

runLexer :: Maybe (Representer Token) -> Maybe SourceName -> LC.ByteString -> Either ParseError ([Token], Representer Token)
runLexer mRepresenter mSrc = runParser lexTokens st src
  where
    src = maybe "" id mSrc
    st = LexState {
        representer = maybe (mkRepresenter id) id mRepresenter
      }

lexTokens :: Lexer ([Token], Representer Token)
lexTokens = do
  spaces
  toks <- many $ lexToken <* spaces
  eof
  st <- getState
  return (mergeMinusSigns toks, representer st)

lexToken :: Lexer Token
lexToken = first [
    lexComment
  , try lexDirective
  , lexSymbol
  , lexBool
  , lexChar
  , lexKeyword
  , lexNumber
  , lexString
  , lexIdentifier
  ] <?> ""

--------------------------------------------------------------------------------

mergeMinusSigns :: [Token] -> [Token]
mergeMinusSigns [] = []
mergeMinusSigns [t] = [t]
mergeMinusSigns (t:TSymbol ")":TSymbol "-":TNumber n:ts)
  | isCastLike t = t:TSymbol ")": mergeMinusSigns (TNumber (negate n):ts)
  | otherwise = t : mergeMinusSigns (TSymbol ")":TSymbol "-":TNumber n:ts)
mergeMinusSigns [t1, t2] = case tryNegate t1 t2 of
  Just t -> [t]
  Nothing -> [t1, t2]
mergeMinusSigns (t0:t1:t2:ts)
  | canNegateAfter t0 = case tryNegate t1 t2 of
      Just t -> t0 : mergeMinusSigns (t : ts)
      Nothing -> t0 : mergeMinusSigns (t1 : t2 : ts)
  | otherwise = t0 : mergeMinusSigns (t1 : t2 : ts)

isCastLike :: Token -> Bool
isCastLike t = case t of
  TKeyword _ -> True
  TSymbol sym -> sym == "*"
  _ -> False

tryNegate :: Token -> Token -> Maybe Token
tryNegate (TSymbol "-") (TNumber x) = Just $ TNumber $ negate x
tryNegate _ _ = Nothing

canNegateAfter :: Token -> Bool
canNegateAfter t = case t of
  TDirective _ -> True
  TKeyword _ -> True
  TSymbol sym -> sym `elem` validSyms
  _ -> False
  where
    validSyms = [
        "{", "}", "[", "(", "<", ">", "<=", ">=", "-", "*", "/"
      , "~", "!", "%", "^", "&", "|", "<<", ">>", "&&", "||"
      , "==", "!=", "=", "+=", "-=", "*=", "/=", "%=", "<<="
      , ">>=", "&=", "^=", "|=", "?", ":", ",", ";", "#", "##"
      ]

--------------------------------------------------------------------------------

lexComment :: Lexer Token
lexComment = intern =<< fmap TComment (blockComment <|> lineComment)

lineComment :: Lexer TS.StringType
lineComment = pack $ do
  _ <- try $ string "//"
  noneOf "\r\n" `manyTill` (eol <|> eof)

blockComment :: Lexer TS.StringType
blockComment = pack $ do
  _ <- try $ string "/*"
  blockComment'

blockComment' :: Lexer String
blockComment' = do
  c <- anyChar <?> "closing comment"
  let continue = fmap (c:) blockComment'
  case c of
    '*' -> (char '/' >> return "") <|> continue
    _ -> continue

--------------------------------------------------------------------------------

lexBool :: Lexer Token
lexBool = intern . TBool =<< (wholeWord $ lexTrue <|> lexFalse)
  where
    lexTrue = try (string "true") >> return True
    lexFalse = try (string "false") >> return False

--------------------------------------------------------------------------------

unescape :: Char -> Char
unescape c = case c of
  'a' -> '\a'
  'b' -> '\b'
  'f' -> '\f'
  'n' -> '\n'
  'r' -> '\r'
  't' -> '\t'
  'v' -> '\v'
  '\''-> '\''
  '?' -> '?'
  '"' -> '"'
  '\\'-> '\\'
  _ -> error $ "Cannot escape \\" ++ [c]

charToInt :: Char -> Int
charToInt c
  | '0' <= k && k <= '9' = ord k - ord '0'
  | 'a' <= k && k <= 'z' = ord k - ord 'a' + 10
  | otherwise = error $ "C.Lexer.charToInt: Bad argument " ++ show c
  where
    k = toLower c

surround :: Char -> Lexer a -> Lexer a
surround c = between (char c) (char c)

intDigit :: NumeralSystem -> Lexer Int
intDigit system = do
  c <- satisfy (inSystem system) <?> errorMsg
  return $ charToInt c
  where
    errorMsg = case system of
      Oct -> "octal digit"
      Dec -> "decimal digit"
      Hex -> "hexidecimal digit"

charInNumSystem :: NumeralSystem -> Lexer Char
charInNumSystem sys = fmap (chr . fromDigits' sys) $ sequencer $ intDigit sys
  where
    sequencer p = case sys of
      Oct -> p >>= \res -> fmap (res :) $ atMost 2 p
      _ -> many1 p

--------------------------------------------------------------------------------

lexChar :: Lexer Token
lexChar = intern . TChar =<< (surround delim $ cChar delim)
  where
    delim = '\''

cChar :: Char -> Lexer Char
cChar delim = escapedChar <|> simpleChar delim

simpleChar :: Char -> Lexer Char
simpleChar delim = noneOf $ delim : "\r\n\\"

escapedChar :: Lexer Char
escapedChar = do
  _ <- char '\\'
  first [
      simpleEscape
    , hexEscape
    , octEscape
    ]
  where
    simpleEscape = fmap unescape $ oneOf "abfnrtv'?\"\\"
    hexEscape = oneOf "xX" *> charInNumSystem Hex
    octEscape = charInNumSystem Oct

--------------------------------------------------------------------------------

lexString :: Lexer Token
lexString = (many1 $ lexString' <* spaces) >>= create . fold >>= intern
  where
    create Nothing = mzero
    create (Just s) = return $ TString $ TS.pack s
    fold [] = Nothing
    fold ss = Just $ foldr1 (++) ss

lexString' :: Lexer String
lexString' = surround delim $ many $ cChar '"'
  where
    delim = '"'

--------------------------------------------------------------------------------

lexKeyword :: Lexer Token
lexKeyword = try $ do
  str <- wholeWord $ many1 letter
  case Trie.lookup str keywordTrie of
    Nothing -> mzero
    Just kw -> intern $ TKeyword kw

keywordTrie :: TrieMap Map Char Keyword
keywordTrie = Trie.fromList $ zip (map cshow keywords) keywords

--------------------------------------------------------------------------------

lexSymbol :: Lexer Token
lexSymbol = try $ do
  lSym <- lexSymbol' ""
  case getLast lSym of
    Nothing -> mzero
    Just sym -> intern $ TSymbol $ TS.pack sym

lexSymbol' :: String -> Lexer (Last String)
lexSymbol' cs = flip (<|>) (return $ Last Nothing) $ do
  c <- lookAhead anyChar
  case Trie.lookup (c:cs) symbolTrie of
    Nothing -> return $ Last Nothing
    Just sym -> do
      _ <- anyChar -- eat the earlier lookAhead
      lSym <- lexSymbol' (c:cs)
      return $ Last (Just sym) `mappend` lSym

symbolTrie :: TrieMap Map Char String
symbolTrie = Trie.fromList $ zip (map (reverse . cshow) symbols) symbols

--------------------------------------------------------------------------------

lexDirective :: Lexer Token
lexDirective = intern =<< do
  _ <- char '#'
  lineSpaces
  includeDirective
    <|> (first $ map buildUnary unaryDs)
    <|> (first $ map buildNullary nullaryDs)
  where
    nullaryDs = [DElse, DEndif]
    buildNullary d = do
      _ <- wholeWord $ try $ string $ tail $ directiveName d
      return $ TDirective d
    unaryDs = [DDefine, DUndef, DError, DLine, DIfdef, DIfndef, DIf, DElif]
    buildUnary d = do
      _ <- wholeWord $ try $ string $ tail $ directiveName (d undefined)
      arg <- many (noneOf "\r\n")
      return $ TDirective $ d $ TS.pack arg

includeDirective :: Lexer Token
includeDirective = do
  _ <- try $ string "include"
  lineSpaces
  (path, pathType) <- quoted <|> bracketed
  return $ TDirective $ DInclude pathType $ TS.pack path
  where
    bracketed = do
      path <- between (char '<') (char '>') $ many $ cChar '>'
      return (path, BracketedPath)
    quoted = do
      path <- surround '"' $ many $ cChar '"'
      return (path, QuotedPath)

--------------------------------------------------------------------------------

identifier :: Lexer String
identifier = do
  start <- letter <|> char '_'
  rest <- many $ alphaNum <|> char '_'
  return $ start : rest

lexIdentifier :: Lexer Token
lexIdentifier = do
  i <- identifier
  intern $ TIdentifier $ TS.pack $ removeK i
  where
    -- removeK removes the K in (HC_KOpen -> HC_Open) and
    -- in similar functions (HC_KInsert -> HC_Insert, etc.)
    removeK ('H':'C':'_':'K':c1:c2:cs)
      | isUpper c1 && not (isUpper c2) = "HC_" ++ (c1:c2:cs)
    removeK str = str

--------------------------------------------------------------------------------

lexSignFunc :: Num a => Lexer (a -> a)
lexSignFunc = option id $ (id <$ char '+') <|> (negate <$ char '-')

lexNumber :: Lexer Token
lexNumber = (intern =<<) $ wholeWord $ do
  signFunc <- lexSignFunc
  num <- tryInt <|> decNum
  return $ TNumber $ signFunc $ num
  where
    tryInt = try $ char '0' >> (hexNum <|> octNum)

eatIntSuffix :: Lexer ()
eatIntSuffix = option () $ do
  _ <- (ichar 'u' >> oichar 'l') <|> (ichar 'l' >> oichar 'u')
  return ()
  where
    ichar c = oneOf [toLower c, toUpper c]
    oichar c = option '\0' $ ichar c

eatFloatSuffix :: Lexer ()
eatFloatSuffix = option () $ do
  _ <- (ichar 'f' >> oichar 'l') <|> (ichar 'l' >> oichar 'f')
  return ()
  where
    ichar c = oneOf [toLower c, toUpper c]
    oichar c = option '\0' $ ichar c

octNum :: Lexer Number
octNum = do
  digits <- many octDigit
  notFollowedBy $ char '.'
  eatIntSuffix
  return $ TInteger $ foldl' build 0 $ map charToInt digits
  where
    build tot n = 8 * tot + fromIntegral n

hexNum :: Lexer Number
hexNum = do
  _ <- oneOf "xX"
  digits <- many hexDigit
  notFollowedBy $ char '.'
  eatIntSuffix
  return $ TInteger $ foldl' build 0 $ map charToInt digits
  where
    build tot n = 16 * tot + fromIntegral n

decNum :: Lexer Number
decNum = do
  digits <- many1 digit
  flt <- floating1 <|> floating2 <|> (eatIntSuffix >> return "")
  return $ if null flt
    then TInteger $ read digits
    else TDouble $ fst $ head $ readFloat (digits ++ flt)

floating1 :: Lexer String
floating1 = do
  _ <- oneOf "eE"
  sign <- option "" $ fmap (\x -> [x]) $ oneOf "+-"
  power <- many1 digit
  eatFloatSuffix
  return $ "e" ++ sign ++ power

floating2 :: Lexer String
floating2 = do
  _ <- char '.'
  digits <- option "0" $ many1 digit
  expPart digits <|> do
    eatFloatSuffix
    return ('.' : digits)
  where
    expPart floatPart = do
      _ <- oneOf "eE"
      sign <- option "" $ fmap (\x -> [x]) $ oneOf "+-"
      power <- many1 digit
      eatFloatSuffix
      return $ '.' : floatPart ++ "e" ++ sign ++ power


