{-# LANGUAGE TemplateHaskell #-}

module C.Lexer.Compile.TH (
  compile
) where

import qualified C.Lexer.Compile as L
import Language.Haskell.TH

--------------------------------------------------------------------------------

compile :: String -> ExpQ
compile s = [|L.compile s|]

