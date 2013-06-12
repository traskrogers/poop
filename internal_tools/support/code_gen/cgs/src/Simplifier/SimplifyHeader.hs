module Simplifier.SimplifyHeader (
  simplifyHeader
) where

import Directive (PathType(..), Directive(..))
import Keyword (Keyword(..))
import Token (Token, GenToken(..))
import WildString (WildString(..), (=?))

--------------------------------------------------------------------------------

simplifyHeader :: [Token] -> [Token]
simplifyHeader = (newHeader ++) . stripHeader
  where
    newHeader = [
        TDirective $ DInclude QuotedPath "shared.h"
      , TKeyword KInt
      ]

p (TIdentifier i)
  | i == "main" = False
  | i =? Wild "code_chain_##*" = False
  | otherwise = True
p _ = True

stripHeader :: [Token] -> [Token]
stripHeader = stripHeader' . dropWhile p

stripHeader' :: [Token] -> [Token]
stripHeader' (codeChain:ts) = extern ++ dropWhile p ts
    where
      extern = [
          TKeyword KExtern
        , TKeyword KInt
        , codeChain
        , TSymbol "("
        , TKeyword KVoid
        , TSymbol ")"
        , TSymbol ";"
        ]

