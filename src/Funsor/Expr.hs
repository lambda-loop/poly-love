{-# LANGUAGE TypeFamilies #-}

module Funsor.Expr where

import Genome
import Control.Monad
import qualified System.Random.MWC as MWC
import Data.Ord (Down (Down))
import qualified Data.Vector.Strict as Vec
import Mutation.Vec
import qualified Data.Map.Strict as Map

type Expr = Vec.Vector Op
data Op
  = Inp
  | Lit Integer
  | Add
  | Sub
  | Mul 
  deriving (Eq, Show, Ord)

newOp :: MWC.GenIO -> IO Op  
newOp gen = do 
  op::Int <- MWC.uniformR (0, 4) gen
  case op of
    0 -> pure Inp
    1 -> Lit . fromIntegral <$> MWC.uniformR (-100::Int,100) gen
    2 -> pure Add
    3 -> pure Sub
    _ -> pure Mul

-- TODO: rewrite it in a more vector way..?
eval  :: [Op] -> Integer -> [Integer] -> Integer
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

instance Genome Expr where
  type Score Expr = Down Double

  fit :: Expr -> Fen Expr (Score Expr)
  fit expr = 
    let (g, s) = fitness expr in
      Fen g (Down s)

  done :: Fen Expr (Score Expr) -> Bool
  done (Fen _ score) = score == 0.0

  new :: IO Expr
  new = do
    gen <- MWC.createSystemRandom
    len <- MWC.uniformR (10, 30) gen
    Vec.replicateM len (newOp gen)

  point :: Expr -> MWC.GenIO -> IO Expr
  point expr gen = do 
    b <- MWC.uniform gen
    if b then singlePoint expr gen (newOp gen) 
    else do
      b' <- MWC.uniform gen
      if b' then singleInsert expr gen (newOp gen)
            else singleDelete expr gen 
    
  cross :: Expr -> Expr -> MWC.GenIO -> IO Expr
  cross = altZipMax

-- easy: 2x²- 3x + 10
-- h.Exprard: 23x⁴ - 12x³ + 6x² - 8x + 37
-- harder: 7x⁹−15x⁸+42x⁷+3x⁶−99x⁵+14x⁴−5x³+88x²−2x+101
harder :: Expr
harder = Vec.fromList 
  [ Lit 7, Inp, Mul      -- 7x
  , Lit 15, Sub          -- 7x - 15
  , Inp, Mul             -- 7x^2 - 15x
  
  , Lit 42, Add          -- 7x^2 - 15x + 42
  , Inp, Mul             -- 7x^3 - 15x^2 + 42x
  
  , Lit 3, Add           -- 7x^3 - 15x^2 + 42x + 3
  , Inp, Mul             -- 7x^4 - 15x^3 + 42x^2 + 3x
  
  , Lit 99, Sub          -- 7x^4 - ... - 99
  , Inp, Mul             -- 7x^5 - ... - 99x
  
  , Lit 14, Add          -- 7x^5 - ... + 14
  , Inp, Mul             -- 7x^6 - ... + 14x
  
  , Lit 5, Sub           -- 7x^6 - ... - 5
  , Inp, Mul             -- 7x^7 - ... - 5x
  
  , Lit 88, Add          -- 7x^7 - ... + 88
  , Inp, Mul             -- 7x^8 - ... + 88x
  
  , Lit 2, Sub           -- 7x^8 - ... - 2
  , Inp, Mul             -- 7x^9 - ... - 2x
  
  , Lit 101, Add         -- 7x^9 - 15x^8 + 42x^7 + 3x^6 - 99x^5 + 14x^4 - 5x^3 + 88x^2 - 2x + 101
  ]

hard = Vec.fromList 
  [ Lit 23, Inp, Mul     -- 23x
  , Lit 12, Sub          -- 23x - 12
  , Inp, Mul             -- (23x - 12)x = 23x^2 - 12x
  , Lit 6, Add           -- 23x^2 - 12x + 6
  , Inp, Mul             -- 23x^3 - 12x^2 + 6x
  , Lit 8, Sub           -- 23x^3 - 12x^2 + 6x - 8
  , Inp, Mul             -- 23x^4 - 12x^3 + 6x^2 - 8x
  , Lit 37, Add          -- 23x^4 - 12x^3 + 6x^2 - 8x + 37
  ]
-- answer = easy 
answer :: Expr
answer = hard
easy :: Expr
easy = Vec.fromList 
  [ Inp, Inp, Mul, Lit 2, Mul  
  , Inp, Lit 3, Mul              
  , Sub                          
  , Lit 10                       
  , Add                          
  ]
-- answerSize = length answer

expected :: Map.Map Integer Integer 
expected = Map.fromList [(i, aux i) | i :: Integer <- is]
  where 
    aux :: Integer -> Integer
    aux i = eval (Vec.toList answer) (fromIntegral i) []
        
-- standart derivation
fitness :: Expr -> (Expr, Double)
fitness expr =
  let expr' = Vec.toList expr
      diffs = do i <- is
                 let out_i = eval expr' i []
                 pure $ out_i - (expected Map.! i)
  in diffs 
    |> fmap (^(2::Int))
    |> sum 
    |> fromIntegral
    |> (/(len-1))
    |> sqrt
    |> (expr,)
    where (|>) = flip ($)
          len  = fromIntegral (length is)


is :: [Integer]
is = [6584,7818,-6192,-8374,8857,-4658,-4514,-9770,-9050,4479,-4169,-5934,271,-1,2801,8883,-6999,9965,5715,7579,7272,1645,3422,263,-4937,-6088,-6259,-2408,5954,-3218,8900,-7034,-4659,3765,145,5986,-1824,-1457,1638,-6169,-8950,-2346,7373,-8498,-8781,-4884,-6846,-7971,-9017,-1807,2346,3397,-4319,5484,3055,1972,2786,-3712,-957,-6486,2193,-3578,-6952,-2021,-7670,3110,7609,-2632,8956,2628,-3560,-4075,2270,-3827,-5529,-8898,3397,-3675,8559,-8114,8406,2985,-7099,2415,7132,-1473,-8135,-8807,-9745,-884,-4761,4164,-7052,6120,4991,-401,-1775,3908,888,4111,-2740,3786,3247,-2322,6898,5869,9242,-3947,3067,5911,-9950,-3303,7657,-1675,-8274,-6274,-5964,2088,7388,1395,-8329,950,9024,-4054,-4147,2031,3487,8873,-1124,-9166,6840,1415,-2268,2488,-6031,4487,4544,2256,-7149,-8494,4239,4563,8661,-130,-7847,-5409,1699,7444,7646,-9084,6477,-772,6209,6675,9565,-7428,7337,2342,-7865,-6357,-6806,-4492,-748,5881,-1612,6678,8175,698,4273,2647,-4496,3682,-7233,3301,3876,-3088,7360,7245,1945,-9542,-2344,-8896,-3309,-6947,9313,-3580,6386,-3857,-4950,5886,-3869,2899,-958,-2343,-6509,-6987,2894,-561,5190,7820,9383,8083,-1115,1963,-7838,8310,-5955,-3903,9876,4501,7135,-3651,653,-3618,331,7316,5098,-4489,-8597,6692,-7771,-2697,-619,527,4404,-6476,3701,7923,5664,8926,4807,4989,-2310,-9279,3159,1199,8287,5482,-9735,-2681,8832,7210,3291,3610,2826,-4741,3592,-5312,3399,2325,-5021,227,9145,-3428,-7961,370,6824,-9011,-6944,-6040,-3608,-3383,7005,5483,-3233,4702,-2758,7582,9349,-7539,-2507,-7814,-9171,-5421,-322,-4780,-9319,6037,-2406,2348,7331,-8464,1775,591,-2565,7046,3698,-4414,9710,4814,4254,8513,1119,-4457,5410,8985,1726,3553,3412,-5264,9421,-1042,5982,9554,-3601,445,-160,4543,4066,-7244,-2356,6338,10000,1584,401,8226,1723,6165,415,-3464,-1874,2899,-971,1331,-6762,-5610,-3123,-8234,7995,-7119,6933,1481,7082,-3392,-2775,2340,-2952,-5643,4923,-3948,-6553,-9635,9341,4976,4569,264,8211,4271,-3251,-137,-6969,-642,-5463,931,-3702,4990,1956,2477,1625,-1891,445,-5669,-9864,-5914,8819,-8450,-9979,-5895,768,-5157,5529,-7486,4691,-5263,5657,-6483,1149,2026,4906,9790,-9434,-8431,-5519,2456,-1809,1108,-398,27,-1216,2972,7571,4791,-9300,3498,2211,964,4609,7549,-5613,-3115,2714,-1218,1859,8516,9696,7887,-1682,-1378,6133,-1928,2407,3246,-5083,-5188,5079,7788,-708,-6737,-6750,-9707,8991,8896,5216,-9592,7263,8199,-3295,-9976,6403,-4445,3898,-5089,2954,-1184,1589,-7192,-9656,269,-1889,-9884,-8076,-387,4525,5860,229,2340,7045,-116,-8009,-3263,-5701,-1732,-3727,4752,1011,591,1734,-8823,6861,-2909,-4718,-9134,-8939,8376,-4193,34,-116,4499,-7021,9686,-8565,922,-7402,3786,9829,-8908,3333,9207,-3341,-1198,-7883,-1338,-3297,-9403,4534,5060,7278,-4668,-147,4765,8207,7402,8689,-3390,2842,-6311,-4116,-4250,-8478,-6182]

