{-# LANGUAGE TypeFamilies #-}

module Genome where

import qualified System.Random.MWC as MWC

data Fen a b = Fen a b deriving (Eq, Show)
class Genome a where
  type Score a
  fit  :: a -> Fen a (Score a)
  done :: Fen a (Score a) -> Bool

  new   :: IO a
  point :: a -> MWC.GenIO -> IO a 
  cross :: a -> a -> MWC.GenIO -> IO a

instance (Eq a, Ord b) => Ord (Fen a b) where
  (Fen _ s) `compare` (Fen _ v) = s `compare` v


