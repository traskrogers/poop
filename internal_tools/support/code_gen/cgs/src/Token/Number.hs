{-# LANGUAGE TemplateHaskell #-}

module Token.Number (
  Number(TInteger, TDouble)
) where

import C.Show (CShow(..))
import Control.DeepSeq (NFData(..))
import Control.Exception (assert)
import Data.Binary
import Data.List.Extra (stripSuffix)
import Language.Haskell.TH (litE, rationalL)
import Language.Haskell.TH.Syntax (Lift(..))

--------------------------------------------------------------------------------

data Number
  = TInteger Integer
  | TDouble Double
  deriving (Show, Read, Eq, Ord)

instance CShow Number where
  cshow (TInteger x) = show x
  cshow (TDouble x) = let
    s = show x
    in maybe s id $ stripSuffix ".0" s

instance Lift Number where
  lift n = case n of
    TInteger i -> [|TInteger i|]
    TDouble d -> [|TDouble $(litE $ rationalL $ toRational d)|]

instance Binary Number where
  put (TInteger n) = putWord8 0 >> put n
  put (TDouble x) = putWord8 1 >> put x
  get = do
    t <- getWord8
    case t of
      0 -> fmap TInteger get
      1 -> fmap TDouble get
      _ -> assert False $ error "Number.get{Number}"

promote :: Number -> Number
promote (TInteger x) = TDouble $ fromInteger x
promote (TDouble x) = TDouble x

lift1 :: (Integer -> Integer) -> (Double -> Double) -> (Number -> Number)
lift1 op _ (TInteger x) = TInteger $ op x
lift1 _ op (TDouble x) = TDouble $ op x

lift2 :: (Integer -> Integer -> Integer) -> (Double -> Double -> Double) -> (Number -> Number -> Number)
lift2 op _ (TInteger x) (TInteger y) = TInteger $ x `op` y
lift2 _ op (TDouble x) (TDouble y) = TDouble $ x `op` y
lift2 op op' x y = lift2 op op' (promote x) (promote y)

instance Num Number where
  (+) = lift2 (+) (+)
  (*) = lift2 (*) (*)
  (-) = lift2 (-) (-)
  negate = lift1 negate negate
  abs = lift1 abs abs
  signum = lift1 signum signum
  fromInteger = TInteger

instance NFData Number where
  rnf (TInteger x) = x `seq` ()
  rnf (TDouble x) = x `seq` ()



