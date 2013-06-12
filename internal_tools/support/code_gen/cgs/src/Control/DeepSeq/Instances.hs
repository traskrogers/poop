{-# OPTIONS_GHC -fno-warn-orphans #-}

module Control.DeepSeq.Instances (
) where

import Control.DeepSeq (NFData(..))
import Data.ByteString as BI
import Data.ByteString.Lazy.Internal as BIL

--------------------------------------------------------------------------------

instance NFData BI.ByteString where
  -- no need to override default behavior


instance NFData BIL.ByteString where
  rnf BIL.Empty       = ()
  rnf (BIL.Chunk _ b) = rnf b


