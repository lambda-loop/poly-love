
module Gen.Evolution.Simple where

-- internal
import Gen.Class

-- external
import GHC.Natural

evolve :: forall γ. Genome γ 
  => Natural -- ^. generation_len
  -> Natural -- ^. max_generations
  -> Maybe Natural -- ^. stuck bound
  -> Natural -- ^. trys
  -> IO γ
evolve size bound stuck_alert trys = do
  let (gen:generators)::[Natural -> IO [γ]] = startGeneration 
  fst_gen <- gen size
  
  -- undefined
  pure x
  
      -- generators = take size 
  -- undefined


