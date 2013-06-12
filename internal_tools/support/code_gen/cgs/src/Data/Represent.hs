{-# LANGUAGE ExistentialQuantification #-}

module Data.Represent (
  Representer
, mkRepresenter
, represent
, representAll
, representatives
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Tuple.Map (lazyMapFst2)

--------------------------------------------------------------------------------

data Representer a = forall k. Ord k => R (a -> k) (Map k a)

mkRepresenter :: Ord k => (a -> k) -> Representer a
mkRepresenter f = R f Map.empty

represent :: a -> Representer a -> (a, Representer a)
represent x r@(R f tbl) = case Map.lookup k tbl of
  Just x' -> (x', r)
  Nothing -> (x, R f $ Map.insert k x tbl)
  where
    k = f x

representAll :: [a] -> Representer a -> ([a], Representer a)
representAll [] table = ([], table)
representAll (x:xs) i = lazyMapFst2 (x':) $ representAll xs i'
  where
    (x', i') = represent x i

representatives :: (Ord a) => [a] -> [a]
representatives xs = fst $ representAll xs $ mkRepresenter id


