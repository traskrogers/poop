{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Haskell.TH.Syntax.LiftExtra (
) where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC
import Language.Haskell.TH.Syntax (Lift(..))

--------------------------------------------------------------------------------

instance Lift C.ByteString where
  lift bs = [| C.pack s |]
    where
      s = C.unpack bs

instance Lift LC.ByteString where
  lift bs = [| LC.pack s |]
    where
      s = LC.unpack bs


