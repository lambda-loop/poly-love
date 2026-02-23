module Main where
import Genome
import Funsor
import qualified Strategy.IslandModel as IM
import qualified Strategy.HallOfFame as HF

main :: IO ()
main = do
  a :: Expr <- IM.evolve 5 20 2
  -- a :: Expr <- HF.evolve 
  print a
  
