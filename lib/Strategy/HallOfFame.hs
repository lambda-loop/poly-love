{-# LANGUAGE RecordWildCards #-}

module Strategy.HallOfFame where

import Genome

import Control.Monad
import Control.Monad.State.Strict

import Control.Concurrent
import Control.Concurrent.STM
import System.Exit

import qualified Data.Heap as Heap
import qualified Data.Vector.Strict as Vect -- TODO: delete_it
import qualified Data.Vector.Strict.Mutable as MVec
import qualified Data.Set as Set
import qualified Data.Foldable as Heap hiding (minimum)

import qualified Data.List as List
import qualified Data.Ord
import qualified System.Random.MWC as MWC
import GHC.IO.Unsafe (unsafePerformIO)


newtype Indexed a = Indexed (Int, a)
  deriving (Eq)

instance (Genome a, Eq a, Ord b) => Ord (Indexed (Fen a b)) where
  Indexed (_, l) `compare` Indexed (_, r) = l `compare` r

-- TODO: type families for vectors with exact size?
data Table a = Table
  { heap :: Heap.Heap (Indexed (Fen a (Score a))) -- WARNING: ugly!
  , vect :: MVec.IOVector a
  } -- deriving (Show) 

tMin :: Table a -> (Score a, Table a)
tMin t = 
  let Indexed (_, Fen _ !min_score) = Heap.minimum (heap t)
  in (min_score, t)

-- N Times :P 
singlesFrom :: Int -> Vect.Vector a -> MWC.GenIO -> IO (Vect.Vector a)
singlesFrom n v gen = do
  let v_len = Vect.length v
  is <- Vect.replicateM n $ MWC.uniformR (0, v_len - 1) gen
  pure $ Vect.map (v Vect.!) is

-- N Times :r
pairsFrom :: Int -> Vect.Vector a -> MWC.GenIO -> IO (Vect.Vector (a, a))
pairsFrom n v gen = do
  let v_len = Vect.length v
  is <- Vect.replicateM n $ MWC.uniformR (0, v_len - 1) gen
  js <- Vect.replicateM n $ MWC.uniformR (0, v_len - 1) gen
  pure $ Vect.zipWith (\i j -> (v Vect.! i, v Vect.! j)) is js

build :: forall a. (Genome a, Ord a, Ord (Score a)) => Vect.Vector a -> IO (Table a)
build vec = do
  m_vec <- Vect.thaw vec
  let !fens = fit <$> Vect.toList vec
      !(gs, fs) = List.unzip $ (\fen@(Fen g _) -> (g, fen)) <$> fens
      !i_ss = Indexed <$> zip [0..] fs

  pure $ Table {
    heap = Heap.fromList i_ss,
    vect = m_vec
  }

insert :: (Genome a, Ord a, Ord (Score a)) => Table a -> Fen a (Score a) -> STM (Table a)
insert t@Table {..} fen@(Fen x score) 
  | Indexed (0, fen)  `Heap.elem` heap = pure t -- 0 is a place holder
  | otherwise = do
    let Indexed (!min_idx, _) = Heap.minimum heap
    !val <- pure . unsafePerformIO $ do 
      val <- MVec.read vect min_idx
      MVec.write vect min_idx x 
      pure val

    let !heap' = Heap.adjustMin 
            (\_ -> Indexed (min_idx, Fen val score)) 
            heap

    pure Table {
      heap = heap',
      vect = vect
    }

type Pop a = StateT (Table a) IO a

-- TODO: change this name..

setup :: (Eq a, Show a, Ord a, Genome a, Eq (Score a), Ord (Score a)) => Int -> IO (Vect.Vector a)
setup n = do
  gs <- replicateM (n*100) new
  pure $ gs |> fmap fit
            |> List.sortOn (Data.Ord.Down . (\(Fen _ b) -> b))
            |> fmap (\(Fen g _) -> g) 
            |> take n
            |> Vect.fromList 

  where (|>) = flip ($)
  

evolve :: forall a. (Eq a, Show a, Ord a, Genome a, Eq (Score a), Show (Score a), Ord (Score a)) => IO a
evolve = do
  let n = 100
  gs::Vect.Vector a <- setup n

  table@Table { heap } <- build gs

  let Indexed (_, Fen _ !r) = Heap.minimum heap 
  t_ruler <- newTVarIO r
  t_vect <- newTVarIO gs

  let queueB = n -- `div` 2
  let threads = 3
  let batch_size = queueB `div` 2 --threads*n*threads

  inserterq <- newTBQueueIO (fromIntegral queueB)

  let table_agent = tableAgent table t_vect t_ruler r inserterq 

  _t0 <- liftIO . forkIO $ table_agent 
  
  replicateM_ threads $ do 
      gen <- MWC.createSystemRandom
      forkIO $ pointAgent t_vect t_ruler  inserterq gen batch_size
  
  replicateM_ threads $ do
      gen <- MWC.createSystemRandom
      forkIO $ crossAgent t_vect t_ruler  inserterq gen batch_size

  _ <- liftIO . forever $ do
    -- threadDelay 1_000_000
    t <- readTVarIO t_vect 
    let l = (fmap fit . Vect.toList) t
    let rv@(best:_) = List.sortOn (Data.Ord.Down . (\(Fen _ b) -> b)) l
    when (done best) $ do
      print "done"
      print best
      -- forM_ [t0,t1,{-t2,{-t3,t4,-} t5, -}t6] killThread
      exitSuccess
    -- let (best:_) = List.sortOn (\(Fen _ b) -> b) l
    putStrLn $ "----------------------------------------------------"
    print . take 20 $ fmap (\(Fen _ b) -> b) rv
    -- putStrLn $ "queue length: " ++ show q_len

  exitFailure

tableAgent :: (Eq a, Ord a, Genome a, Ord (Score a))
  => Table a             
  -> TVar (Vect.Vector a)  -- snapshopt
  -> TVar (Score a)        -- ruler
  -> Score a  
  -> TBQueue (Fen a (Score a))
  -> IO ()

tableAgent table t_vect t_ruler ruler queue = do
  fens <- atomically $ do
     f <- readTBQueue queue
     r <- flushTBQueue queue
     pure (f:r)

  case filter (\(Fen _ s) -> s > ruler) fens of
    [] -> tableAgent table t_vect t_ruler ruler queue
    fens' -> do
      table' <- atomically $ foldM insert table fens'
      let Indexed (_, Fen _ !ruler') = Heap.minimum (heap table')
      snapshot <- Vect.freeze (vect table')

      atomically $ do
        -- n <- lengthTBQueue queue
        -- !_ <- pure . unsafePerformIO $ print n
        writeTVar t_vect snapshot
        writeTVar t_ruler ruler'

      tableAgent table' t_vect t_ruler ruler' queue

pointAgent :: (Genome a, Ord (Score a))
  => TVar (Vect.Vector a)       -- snapshot
  -> TVar (Score a)
  -> TBQueue (Fen a (Score a))
  -> MWC.GenIO
  -> Int                        -- batch size
  -> IO ()
pointAgent t_vect t_ruler queue gen batch_size = do
  vect <- readTVarIO t_vect
  gs <- singlesFrom batch_size vect gen
  ps <- mapM (`point` gen) gs
  !min_score <- readTVarIO t_ruler

  let !candidates = fmap fit ps -- TODO: try with lazy enable
      !approveds  = Vect.filter 
        (\(Fen _ s) -> s > min_score) 
        candidates 

  atomically $ forM_ approveds $ \fen@(Fen _ s) -> do 
    -- when (s > min_score) $ 
    writeTBQueue queue fen


  pointAgent t_vect t_ruler queue gen batch_size

crossAgent :: (Genome a, Ord (Score a))
  => TVar (Vect.Vector a)       -- snapshot
  -> TVar (Score a)
  -> TBQueue (Fen a (Score a))
  -> MWC.GenIO
  -> Int                        -- batch size
  -> IO ()
crossAgent t_vect t_ruler queue gen batch_size = do
  vect <- readTVarIO t_vect
  gs <- pairsFrom batch_size vect gen
  ps <- mapM (\(l, r) -> cross l r gen) gs

  !min_score <- readTVarIO t_ruler

  let !candidates = fmap fit ps -- TODO: try with lazy enable
      !approveds  = Vect.filter 
        (\(Fen _ s) -> s > min_score) 
        candidates 

  atomically $ forM_ approveds $ \fen@(Fen _ s) -> do 
    -- when (s > min_score) $ 
    writeTBQueue queue fen

  crossAgent t_vect t_ruler queue gen batch_size



