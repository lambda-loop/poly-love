
module Mutation.Vec where

import qualified Data.Vector.Strict as Vec
import qualified System.Random.MWC as MWC

import Control.Monad

singlePoint :: Vec.Vector a -> MWC.GenIO -> IO a -> IO (Vec.Vector a)
singlePoint v gen ax = do
  i <- MWC.uniformR (0, Vec.length v - 1) gen
  x <- ax
  pure $ v Vec.// [(i, x)]

singleInsert :: Vec.Vector a -> MWC.GenIO -> IO a -> IO (Vec.Vector a)
singleInsert v gen ax = do
  let v_len = Vec.length v
  i  <- MWC.uniformR (0, v_len - 1) gen
  r  <- MWC.uniformR (0, v_len - i) gen
  xs <- Vec.replicateM (r+1) ax

  let (vl, vr) = Vec.splitAt (i+1) v
  pure $ vl Vec.++ xs Vec.++ vr

singleDelete :: Vec.Vector a -> MWC.GenIO -> IO (Vec.Vector a)
singleDelete v gen = do
  let v_len = Vec.length v
  i <- MWC.uniformR (1, v_len - 1) gen

  let (vl, vr) = Vec.splitAt i v
  b <- MWC.uniform gen

  -- left deletions
  if b then do 
    r <- MWC.uniformR (0, i-1) gen 

    -- putStrLn "left"
    -- print i
    -- print r
    pure $ Vec.take r vl Vec.++ vr
    
  -- right deletions
  else do
    r <- MWC.uniformR (1, v_len - i) gen

    -- putStrLn "right"
    -- print i
    -- print r
    pure $ vl Vec.++ Vec.drop r vr

altZipMin :: Vec.Vector a -> Vec.Vector a -> MWC.GenIO -> IO (Vec.Vector a)
altZipMin vl vr gen = do
  let len = min (Vec.length vl) (Vec.length vr)
  v <- forM [0..(len-1)] $ \i -> do
    b <- MWC.uniform gen
    pure $ if b 
      then vl Vec.! i
      else vr Vec.! i
  (pure . Vec.fromList) v
  
altZipMax :: Vec.Vector a -> Vec.Vector a -> MWC.GenIO -> IO (Vec.Vector a)
altZipMax vl vr gen = do
  undefined

deletionTest = do
  gen <- MWC.createSystemRandom
  let v::Vec.Vector Int = Vec.fromList [0..10]
  -- xs <- Vec.replicateM 100_000 $ singleDelete v gen
  xs <- Vec.replicateM 3 $ singleDelete v gen
  print xs
  -- print (Vec.length v)
  -- (print . Vec.filter (>10)) (Vec.map Vec.length xs)





  

