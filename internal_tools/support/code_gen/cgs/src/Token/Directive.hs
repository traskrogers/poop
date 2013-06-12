{-# LANGUAGE TemplateHaskell #-}

module Token.Directive (
  PathType(..)
, Directive(..)
, directiveName
) where

import C.Show
import Control.Exception (assert)
import Control.DeepSeq (NFData(..), deepseq)
import Data.Binary
import Language.Haskell.TH (conE, mkName)
import Language.Haskell.TH.Syntax (Lift(..))

--------------------------------------------------------------------------------

data PathType
  = QuotedPath
  | BracketedPath
  deriving (Show, Read, Eq, Ord)

instance Lift PathType where
  lift = conE . mkName . show

instance Binary PathType where
  put QuotedPath = putWord8 0
  put BracketedPath = putWord8 1
  get = do
    t <- getWord8
    case t of
      0 -> return QuotedPath
      1 -> return BracketedPath
      _ -> assert False $ error "Directive.get{PathType}"

data Directive s
  = DInclude PathType s
  | DDefine s
  | DUndef s
  | DError s
  | DLine s
  | DIfdef s
  | DIfndef s
  | DIf s
  | DElse
  | DElif s
  | DEndif
  deriving (Show, Read, Eq, Ord)

instance Lift a => Lift (Directive a) where
  lift d = case d of
    DInclude p s -> [|DInclude p s|]
    DDefine s -> [|DDefine s|]
    DUndef s -> [|DUndef s|]
    DError s -> [|DError s|]
    DLine s -> [|DLine s|]
    DIfdef s -> [|DIfdef s|]
    DIfndef s -> [|DIfndef s|]
    DIf s -> [|DIf s|]
    DElse -> [|DElse|]
    DElif s -> [|DElif s|]
    DEndif -> [|DEndif|]

instance Binary s => Binary (Directive s) where
  put d = case d of
    DInclude p s -> putWord8 0 >> put p >> put s
    DDefine s -> putWord8 1 >> put s
    DUndef s -> putWord8 2 >> put s
    DError s -> putWord8 3 >> put s
    DLine s -> putWord8 4 >> put s
    DIfdef s -> putWord8 5 >> put s
    DIfndef s -> putWord8 6 >> put s
    DIf s -> putWord8 7 >> put s
    DElse -> putWord8 8
    DElif s -> putWord8 9 >> put s
    DEndif -> putWord8 10
  get = do
    t <- getWord8
    case t of
      0 -> get >>= \p -> fmap (DInclude p) get
      1 -> fmap DDefine get
      2 -> fmap DUndef get
      3 -> fmap DError get
      4 -> fmap DLine get
      5 -> fmap DIfdef get
      6 -> fmap DIfndef get
      7 -> fmap DIf get
      8 -> return DElse
      9 -> fmap DElif get
      10 -> return DEndif
      _ -> assert False $ error "Directive.get{Binary s => Directive s}"

instance NFData s => NFData (Directive s) where
  rnf d = case d of
    DInclude p s -> p `seq` s `deepseq` ()
    DDefine s -> s `deepseq` ()
    DUndef s -> s `deepseq` ()
    DError s -> s `deepseq` ()
    DLine s -> s `deepseq` ()
    DIfdef s -> s `deepseq` ()
    DIfndef s -> s `deepseq` ()
    DIf s -> s `deepseq` ()
    DElse -> ()
    DElif s -> s `deepseq` ()
    DEndif -> ()

instance Functor Directive where
  fmap f d = case d of
    DInclude pt s -> DInclude pt $ f s
    DDefine s -> DDefine $ f s
    DUndef s -> DUndef $ f s
    DError s -> DError $ f s
    DLine s -> DLine $ f s
    DIfdef s -> DIfdef $ f s
    DIfndef s -> DIfndef $ f s
    DIf s -> DIf $ f s
    DElse -> DElse
    DElif s -> DElif $ f s
    DEndif -> DEndif

instance (CShow s) => CShow (Directive s) where
  cshow d = case d of
    DInclude QuotedPath s -> "#include \"" ++ cshow s ++ "\""
    DInclude BracketedPath s -> "#include <" ++ cshow s ++ ">"
    DDefine s -> "#define " ++ cshow s
    DUndef s -> "#undef " ++ cshow s
    DError s -> "#error " ++ cshow s
    DLine s -> "#line " ++ cshow s
    DIfdef s -> "#ifdef " ++ cshow s
    DIfndef s -> "#ifndef " ++ cshow s
    DIf s -> "#if " ++ cshow s
    DElse -> "#else"
    DElif s -> "#elif " ++ cshow s
    DEndif -> "#endif"

directiveName :: Directive s -> String
directiveName d = case d of
  DInclude _ _ -> "#include"
  DDefine _ -> "#define"
  DUndef _ -> "#undef"
  DError _ -> "#error"
  DLine _ -> "#line"
  DIfdef _ -> "#ifdef"
  DIfndef _ -> "#ifndef"
  DIf _ -> "#if"
  DElse -> "#else"
  DElif _ -> "elif"
  DEndif -> "#endif"








