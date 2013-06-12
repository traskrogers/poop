module Data.Scope (
  Scope(..)
) where

--------------------------------------------------------------------------------

data Scope a
  = Global
  | Local a -- Note: Locally scoped keys are not inherited by children in HOOPS.
  deriving (Show, Eq, Ord)


