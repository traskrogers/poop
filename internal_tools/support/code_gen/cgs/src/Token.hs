module Token (
  Token
, module Token.GenToken
) where

import Token.GenToken
import Token.String (StringType)

--------------------------------------------------------------------------------

type Token = GenToken StringType


