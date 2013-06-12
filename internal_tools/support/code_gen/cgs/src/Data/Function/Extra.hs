module Data.Function.Extra (
  const2
, dot
) where

--------------------------------------------------------------------------------

infixr 9 `dot`

const2 :: a -> b -> c -> a
const2 x _ _ = x

dot :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
dot f g x y = f $ g x y


