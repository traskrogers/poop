{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Hoops.SegPath (
  SegPath
, WildSegPath
, mkPath
, mkWildPath
, (<+>)
, isSimplePath
, pathInit
, pathInits
, expandAlias
, expandPathKeys
, isAbsolute
, isAnonymous
, isAlias
) where

import Control.Exception (assert)
import Control.Monad (MonadPlus(..), guard)
import Data.Char (toLower)
import Data.List (findIndices, group, inits)
import Data.Map (Map)
import Data.Scope (Scope(..))
import qualified Data.Map as Map
import Data.Maybe (maybeToList)
import Hoops.Key (Key, key1, key2)
import Numeric (showHex, readHex)
import Prelude hiding (error)
import qualified Prelude
import Text.Parsec
import qualified Token.String as TS

--------------------------------------------------------------------------------

error :: String -> String -> a
error func msg = Prelude.error $ "SegPath." ++ func ++ ": " ++ msg

--------------------------------------------------------------------------------

data PathPart
  = Root
  | Parent
  | Current
  | Child (Maybe TS.StringType)
  | Alias TS.StringType
  | KeyName Integer
  | Wild WildPart
  deriving (Show, Eq, Ord)

-- So far only supports very trivial uses of wildcards.
-- Each wildcard has to be by itself.
-- Can have "a/*/b", but not "a/*b".
data WildPart
  = WildAtom WildAtom
  | WildComposite [Either WildAtom TS.StringType]
  deriving (Show, Eq, Ord)

data WildAtom
  = WildChar
  | WildString
  | WildDeepString
  deriving (Show, Eq, Ord)

instance TS.Show PathPart where
  show part = case part of
    Root -> "/"
    Parent -> ".."
    Current -> "."
    Child (Just name) -> pathEscape name
    Child Nothing -> Prelude.error "Token.String.Show.show{PathPart}: Cannot show anonymous PathPart."
    Alias name -> '?' `TS.cons` pathEscape name
    KeyName key -> TS.pack $ '@' : showHex key ""
    Wild wp -> TS.show wp

instance TS.Show WildPart where
  show wp = case wp of
    WildAtom wa -> TS.show wa
    WildComposite wc -> TS.concat $ map f wc
      where
        f (Left wa) = TS.show wa
        f (Right s) = pathEscape s

instance TS.Show WildAtom where
  show wa = case wa of
    WildChar -> "%"
    WildString -> "*"
    WildDeepString -> "..."

pathEscape :: TS.StringType -> TS.StringType
pathEscape str
  | reqEsc = TS.concat ["`", TS.concatMap escQuote str, "`"]
  | otherwise = str
  where
    escQuote '`' = "``"
    escQuote c = TS.singleton c
    reqEsc = " " `TS.isPrefixOf` str
          || not (TS.all segChar str)
          || "  " `TS.isInfixOf` str
          || " " `TS.isSuffixOf` str

--------------------------------------------------------------------------------

newtype GenPath phantom = SP [PathPart]
  deriving (Eq, Ord)

data PlainTag
type SegPath = GenPath PlainTag

data WildTag
type WildSegPath = GenPath WildTag

instance Show (GenPath phantom) where
  show = TS.unpack . TS.show

instance TS.Show (GenPath phantom) where
  show (SP parts) = case parts of
    [] -> assert False $ Prelude.error "Token.String.Show.show{SegPath}: Internal error."
    [Child Nothing] -> ""
    [Root] -> "/"
    Root : ps -> '/' `TS.cons` TS.show (SP ps)
    _ -> TS.intercalate "/" $ map TS.show parts

--------------------------------------------------------------------------------

mkPath :: TS.StringType -> SegPath
mkPath str = case runLexer str of
  Left err -> error "mkPath" $ show str ++ "\n" ++ show err
  Right parts -> SP $ validate $ canonicalize parts
  where
    validate ps = if any isWild ps
      then error "mkPath" "Cannot have wildcards in SegPath"
      else ps

mkWildPath :: TS.StringType -> WildSegPath
mkWildPath str = case runLexer str of
  Left err -> error "mkWildPath" $ show str ++ "\n" ++ show err
  Right parts -> SP $ validate $ canonicalize parts
  where
    validate ps = if any (== Child Nothing) ps
      then error "mkWildPath" "Cannot make WildPath that has an anonymous path part in it."
      else ps

matchPath :: WildSegPath -> SegPath -> Bool
matchPath (SP ws) (SP ps)
  | isSimplePath (SP ps) = match ws ps
  | otherwise = error "matchPath" "Cannot try to match non-simple path (see isSimplePath)."

match :: [PathPart] -> [PathPart] -> Bool
match (q:qs) (p:ps) = case q of
  Wild wp -> case wp of
    WildAtom wa -> case wa of
      WildChar -> case p of
        Child (Just str) -> TS.length str == 1 && match qs ps
        _ -> False
      WildString -> match qs ps
      WildDeepString -> match (q:qs) ps || match qs ps || match qs (p:ps)
  _ -> q == p && match qs ps
match [] [] = True
match _ _ = False

isWild :: PathPart -> Bool
isWild (Wild {}) = True
isWild _ = False

isSimplePath :: SegPath -> Bool
isSimplePath (SP ps) = all pred ps
  where
    pred Root = True
    pred (Child (Just _)) = True
    pred _ = False

isAbsolute :: SegPath -> Maybe Bool
isAbsolute (SP parts) = case parts of
  Root : _ -> Just True
  Alias _ : _ -> Just True
  _ -> if any isKeyName parts
    then Nothing
    else Just False

isAliasPart :: PathPart -> Bool
isAliasPart (Alias {}) = True
isAliasPart _ = False

isKeyName :: PathPart -> Bool
isKeyName (KeyName {}) = True
isKeyName _ = False

isAlias :: SegPath -> Bool
isAlias (SP (p:_)) = isAliasPart p
isAlias _ = False

isAnonymousPart :: PathPart -> Bool
isAnonymousPart (Child Nothing) = True
isAnonymousPart _ = False

isAnonymous :: SegPath -> Bool
isAnonymous (SP [Child Nothing]) = True
isAnonymous _ = False

(<+>) :: SegPath -> SegPath -> SegPath
anon @ (SP [Child Nothing]) <+> SP [Current] = anon
SP [Current] <+> anon @ (SP [Child Nothing]) = anon
sp1 @ (SP ps) <+> sp2 @ (SP qs)
  | any isAnonymous [sp1, sp2] = error "(<+>)" "Can only join anonymous segment with \".\"."
  | isAbsolute sp2 == Just True = error "(<+>)" $ "Cannot append absolute path to path: " ++ show ps ++ ", " ++ show qs
  | otherwise = SP . canonicalize $ ps ++ qs

pathInit :: SegPath -> Maybe SegPath
pathInit (SP parts) = case init parts of
  [] -> Nothing
  rest -> Just $ SP rest

pathInits :: SegPath -> [SegPath]
pathInits (SP parts) = case Data.List.inits parts of
  _ : rest -> map SP rest
  [] -> assert False $ error "pathInits" "internal error"

canonicalize :: [PathPart] -> [PathPart]
canonicalize ps = if ps == ps'
  then ps
  else canonicalize ps'
  where
    ps' = canonicalize' ps

canonicalize' :: [PathPart] -> [PathPart]
canonicalize' ps = case ps of
  [Current] -> [Current]
  Current : rest -> rest
  p : Current : [] -> [p]
  Child _ : Parent : rest -> Current : rest
  p : rest -> p : canonicalize' rest
  [] -> []

expandAlias :: Map SegPath SegPath -> SegPath -> SegPath
expandAlias m (SP parts) = case parts of
  a @ (Alias _) : rest -> case Map.lookup (SP [a]) m of
    Nothing -> SP parts
    Just sp -> expandAlias m sp <+> SP rest
  _ -> SP parts

expandPathKeys :: Map (Key, Scope SegPath) SegPath -> Maybe SegPath -> SegPath -> Maybe SegPath
expandPathKeys m mCurr s@(SP ps) = case span (not . isKeyName) ps of
  (qs, KeyName key : rest) -> let
    curr = if isAbsolute (SP qs) == Just True
      then SP qs
      else maybe (SP []) id mCurr
    lookupKey scope = case Map.lookup (key1 key, scope) m of
      Nothing -> Map.lookup (key2 key 0, scope) m
      justPath -> justPath
    expandRest path = expandPathKeys m mCurr $ path <+> SP rest
    in case lookupKey $ Local curr of
      Just path -> expandRest path
      Nothing -> case lookupKey Global of
        Just path -> expandRest path
        Nothing -> Nothing
  _ -> Just s

--------------------------------------------------------------------------------

data QuoteType = UnQuoted | Quoted
  deriving (Eq, Ord, Show)

type Lexer a = TS.GenParser Char () a

runLexer :: TS.StringType -> Either ParseError [PathPart]
runLexer str = flip run str' $ try anon <|> do
  mRoot <- option Nothing $ fmap Just root
  toks <- p `sepBy` pathDelim
  eof
  let toks' = maybeToList mRoot ++ toks
  validate toks'
  return toks'
  where
    str' = trimEnd $ trimFront str
    anon = fmap (:[]) anonymousChild
    p =   try wild
      <|> try parent
      <|> try current
      <|> try namedChild
      <|> alias
      <|> keyName

validate :: [PathPart] -> Lexer ()
validate (Root : toks) = case findIndices isAliasPart toks of
  [] -> return ()
  _ -> fail "Cannot have an alias in rooted path."
validate toks = case findIndices isAliasPart toks of
  [] -> return ()
  [0] -> return ()
  0 : _ -> fail "Cannot have an alias in an already aliased path."
  _ -> fail "Cannot have an alias in relative path."

run :: Lexer a -> TS.StringType -> Either ParseError a
run lexer = runParser lexer () ""

endOfSeg :: Lexer Bool
endOfSeg = try $ do
  spaces
  is eof <|> is pathDelim <|> return False
  where
    is p = p >> return True

pathDelim :: Lexer ()
pathDelim = char '/' >> return ()

quotedText :: Lexer (TS.StringType, QuoteType)
quotedText = do
  q <- oneOf "`'\""
  let till = try $ char q >> notFollowedBy (char q)
      notQ = satisfy (/= q)
      escQ = try (string [q, q]) >> return q
  txt <- manyTill (notQ <|> escQ) $ lookAhead till
  _ <- char q
  return (TS.pack txt, Quoted)

segChar :: Char -> Bool
segChar c
  | 'a' <= c && c <= 'z' = True
  | 'A' <= c && c <= 'Z' = True
  | '0' <= c && c <= '9' = True
  | c `elem` "# +-$_.:" = True
  | otherwise = False

trimFront :: TS.StringType -> TS.StringType
trimFront = TS.dropWhile (== ' ')

trimEnd :: TS.StringType -> TS.StringType
trimEnd = TS.reverse . trimFront . TS.reverse

collapseSpaces :: String -> String
collapseSpaces = concat . map shrink . group
  where
    shrink "" = ""
    shrink str
      | all (== ' ') str = " "
      | otherwise = str

unQuotedText :: Lexer (TS.StringType, QuoteType)
unQuotedText = do
  txt <- many1 $ satisfy segChar
  return (TS.pack $ collapseSpaces $ map toLower txt, UnQuoted)

segName :: Lexer TS.StringType
segName = do
  txt_q_s <- many (unQuotedText <|> quotedText)
  let txt = TS.concat $ trimTexts txt_q_s
  guard $ txt /= ""
  return txt

trimTexts :: [(TS.StringType, QuoteType)] -> [TS.StringType]
trimTexts = map fst . tryMapLast trimEnd . tryMapHead trimFront
  where
    tryMapHead f ((txt, UnQuoted) : rest) = (f txt, UnQuoted) : rest
    tryMapHead _ txt_q_s = txt_q_s
    tryMapLast f = reverse . tryMapHead f . reverse

anonymousChild :: Lexer PathPart
anonymousChild = spaces >> eof >> return (Child Nothing)

root :: Lexer PathPart
root = char '/' >> return Root

alias :: Lexer PathPart
alias = char '?' >> fmap Alias segName

namedChild :: Lexer PathPart
namedChild = fmap (Child . Just) segName

parent :: Lexer PathPart
parent = do
  _ <- string ".." <|> string "^"
  b <- lookAhead endOfSeg
  if b
    then return Parent
    else mzero

current :: Lexer PathPart
current = do
  _ <- char '.'
  b <- lookAhead endOfSeg
  if b
    then return Current
    else mzero

keyName :: Lexer PathPart
keyName = do
  spaces >> char '@' >> spaces
  hexKey <- many1 hexDigit
  spaces
  return $ KeyName $ case readHex hexKey of
    (key, _) : _ -> key
    _ -> assert False $ error "keyName" "Internal error."

wild :: Lexer PathPart
wild = fmap Wild p
  where
    p =   try wildDeepString
      <|> wildString
      <|> wildChar

wildChar :: Lexer WildPart
wildChar = char '%' >> return (WildAtom WildChar)

wildString :: Lexer WildPart
wildString = char '*' >> return (WildAtom WildString)

wildDeepString :: Lexer WildPart
wildDeepString = do
  _ <- string "..."
  b <- lookAhead endOfSeg
  if b
    then return (WildAtom WildDeepString)
    else mzero

