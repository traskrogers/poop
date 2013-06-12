{-# LANGUAGE TypeSynonymInstances #-}

module C.Show (
  CShow(..)
) where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC

--------------------------------------------------------------------------------

class CShow a where
  cshow :: a -> String

instance CShow String where
  cshow = id

instance CShow C.ByteString where
  cshow = C.unpack

instance CShow LC.ByteString where
  cshow = LC.unpack

instance CShow Bool where
  cshow True = "true"
  cshow False = "false"

instance CShow Integer where
  cshow = show


