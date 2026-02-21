{-# LANGUAGE TypeFamilies #-}

module Funsor where

import Genome
import qualified System.Random.MWC as MWC
import Data.Ord (Down)

data Expr 
  = Inp
  | Lit Integer
  | Add
  | Sub
  | Mul 
  -- | Pow
  deriving (Eq, Show)

eval  :: [Expr] -> Integer -> [Integer] -> Integer
eval  [] _ []    = 0
eval  [] _ (x:_) = x

eval  (Inp:gs) inp stack   = eval gs inp (inp:stack)
eval  (Lit x:gs) inp stack = eval gs inp (x:stack)

eval  (Add:gs) inp []          = eval gs inp [0]
eval  (Add:gs) inp [x]         = eval gs inp [x]
eval  (Add:gs) inp (x:y:stack) = eval gs inp (y+x:stack)

eval  (Sub:gs) inp []          = eval gs inp [0]
eval  (Sub:gs) inp [x]         = eval gs inp [-x]
eval  (Sub:gs) inp (x:y:stack) = eval gs inp (y-x:stack)

eval  (Mul:gs) inp []          = eval gs inp [1]
eval  (Mul:gs) inp [x]         = eval gs inp [x]
eval  (Mul:gs) inp (x:y:stack) = eval gs inp (y*x:stack)

-- eval  (Pow:gs) inp []          = eval gs inp [1]
-- eval  (Pow:gs) inp [x]         = eval gs inp [x] 
-- eval  (Pow:gs) inp (x:y:stack) = eval gs inp (y ^ x:stack)


instance Genome Expr where
  type Score Expr = Down Double

  fit :: Expr -> Fen Expr (Score Expr)
  fit = undefined

  done :: Fen Expr (Score Expr) -> Bool
  done = undefined

  new :: IO Expr
  new = undefined

  point :: Expr -> MWC.GenIO -> IO Expr
  point = undefined

  cross :: Expr -> Expr -> MWC.GenIO -> IO Expr
  cross = undefined

