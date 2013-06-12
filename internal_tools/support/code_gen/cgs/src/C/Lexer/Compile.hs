module C.Lexer.Compile (
  compile
) where

import C.Lexer (runLexer)
import Data.String (fromString)
import Token (Token)

--------------------------------------------------------------------------------

compile :: String -> [Token]
compile s = fst . left . runLexer Nothing Nothing $ fromString s
  where
    left = either (error . showMsg) id
    showMsg msg = "CLexer.Compile.compile failed with: " ++ show (s, msg)


