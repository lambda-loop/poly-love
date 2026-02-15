
{-# LANGUAGE StarIsType #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DatatypeContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Gen.Class where

import qualified System.Random.MWC as MWC
import GHC.Natural (Natural)

class Genome α where
  type Score α
  -- u can use unsafePerformIO, but the answers'll always match
  fit             :: [α -> Fen α (Score α)]
  startGeneration :: [Natural -> IO [α]]                                -- nularys
  -- the amount of generated children'll depend on the size of the traversal they got
  pointers :: Traversable τ => [τ α -> MWC.GenIO -> IO (τ α)]        -- unarys
  crossers :: Traversable τ => [τ α -> τ α -> MWC.GenIO -> IO (t α)] -- binarys
  
  -- in the snake case, the Fen is also important because 
  -- rerunning the fit could not lead to the same result
  -- u can use unsafePerformIO, but the answers should always match
  done :: Fen α (Score α) -> Bool

data Fen α β = Fen α β 
  deriving (Eq, Show)

instance (Eq α, Ord β) => Ord (Fen α β) where
  (Fen _ s) `compare` (Fen _ v) = s `compare` v



