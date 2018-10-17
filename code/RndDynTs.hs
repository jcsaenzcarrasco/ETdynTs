{-# LANGUAGE MultiParamTypeClasses #-}

module RndDynTs  where

import EFsets
import Data.FingerTree hiding (null) 
import qualified Data.FingerTree as FT 
import qualified Data.Set as S
import Data.Set (Set,member,fromList)
import Data.Monoid
import System.Random 


-- | Generate a list of ranges between the size of the forest and
-- | the size of the trees
range :: Integral a => (a,a) -> a -> [(a, a)] 
range (from,to) size = range'' (from',to') stop size
 where
  from' = if from > to   then to else from
  to'   = if to   > from then to else from  
  stop  = from'-1  

range'' (from,to) stop size 
  | low+(div total size) <= stop = []
  | otherwise                    = (low,to) : range'' (low-1-size, low-1) stop size 
  where
    total    = to - from
    subrange = from + total - size 
    low      = if subrange <= (from-1) then from else subrange


-- | Initialise a forest with singleton trees 
-- | 
singletons' :: Ord a => [a]        -- List of nodes
                     -> ForestEF a -- from forest 
                     -> ForestEF a -- to   forest 
singletons' [] forest = forest
singletons' (node:nodes) forest -- forest@(EtForest ft) 
  | nodeInF (node,node) forest = singletons' nodes forest -- node already in forest 
  | otherwise           = singletons' nodes (insert (node,node) forest)
 where
   insert :: Ord a => (a,a) -> ForestEF a -> ForestEF a
   insert node forest = ( ( node <| FT.empty ) <| forest )

singletons :: (Enum a,Random a,Num a,Ord a) => (a,a) -> Int -> ForestEF a -> ForestEF a 
singletons (from,to) seed forest =
  singletons' (unique (from',to') seed) forest
 where
   from' = if from > to then to else from
   to'   = if to > from then to else from 


-- | Application of a sequence of operations WITH nodes given separately
-- | 
dynOpsNodes :: Ord a
       => [a -> a -> ForestEF a -> ForestEF a] -- Operations link | cut 
       -> [a]                                  -- [random] nodes 
       -> ForestEF a                           -- from forest 
       -> ForestEF a                           -- to   forest 
dynOpsNodes []       _                   forest = forest 
dynOpsNodes _        []                  forest = forest 
dynOpsNodes _        [node]              forest = forest 
dynOpsNodes (op:ops) (node1:node2:nodes) forest = 
    let forest' = (op node1 node2) forest
    in  dynOpsNodes ops nodes forest' 


-- | Random Forest generator function
-- | the tree size should be smaller than the total number
-- | of nodes, otherwise no forest is generated
-- | 
rndForest :: (Int,Int)    -- segment of nodes in the forest
          -> SizeTree     -- max size of trees in the segment (edge number)
          -> RndSeed      -- random seed
          -> ForestEF Int -- input forest
          -> ForestEF Int -- output: random forest
rndForest _         0    _    forest = forest
rndForest (from,to) size seed forest
 | size >  total  = forest
 | size == total  = dynOpsNodes (opL (10*total)) (ndL (10*total) (from', to') ) initForest 
 | otherwise      = if   null ranges
                    then remainNds 
                    else bldF (length ranges) initForest
 where
   from'         = if from > to   then to else from
   to'           = if to   > from then to else from  
   total         = abs (to - from) 
   ranges        = range (from',to') size             -- list of subranges 
   initForest    = singletons (from',to') seed forest -- initialise forest

   remainNds     = dynOpsNodes (opL size) (ndL (10*size) (from',to')) initForest

   bldF 0 forest = forest
   bldF n forest = bldF (n-1) (dynOpsNodes (opL (size+1))
                                           (ndL (10*size) (ranges!!(n-1)) )
                                           forest )

   opL k           = take    k (repeat link)    -- list of operations 
   ndL k (from,to) = rndList k (from,to) seed   -- random list of nodes


-- | Generic random list: for any case of nodes or operations 
-- | 
rndList :: (Num a,Ord a,Random a)=> Int -> (a,a) -> RndSeed -> [a]
rndList 0 _         _    = []
rndList n (from,to) seed = take n (randomRs (from,to) (mkStdGen seed))

 
-- | Operations (link,cut) random list 

rndOps :: (Num a, Ord a, Random a)
       => Int -> RndSeed
       -> [a -> a -> ForestEF a -> ForestEF a]
rndOps 0  _    = []
rndOps n  seed = mymap $ rndList n (0,1) seed
 where 
   mymap :: Num a => Ord a => [a] -> [a -> a -> ForestEF a -> ForestEF a]
   mymap [] = []
   mymap (x:xs) = if x==0 then link : mymap xs else cut : mymap xs 


-- | Operations (link,cut) random list
-- | Operations as strings (Eval. or Debugging purposes) 

rndOpsS :: Int -> RndSeed -> [String]
rndOpsS 0  _    = []
rndOpsS n  seed = mymap $ (rndList n (0,1) seed :: [Int])
 where 
   mymap :: (Eq a, Num a) => [a] -> [String]
   mymap [] = []
   mymap (x:xs) = if x==0 then "link" : mymap xs else "cut" : mymap xs 



-- ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
--
--            HELPER functions  (for trees and forest)
--

newtype Elem a = Elem a deriving (Show)
data FTx a = FTx (FingerTree (S.Set a) (Elem a)) deriving Show

instance Ord a => Measured (S.Set a) (Elem a) where
  measure (Elem x) = S.singleton x

type RndSeed  = Int
type SizeTree = Int 

toListFTx :: Ord a => FTx a -> [a]
toListFTx (FTx ft) = case (viewl ft) of
  EmptyL         -> []
  Elem x :< rest -> x : toListFTx (FTx rest)

unique :: (Enum a,Num a,Ord a,Random a)=> (a,a) -> Int -> [a]
unique (from,to) seed = toListFTx $ insFT total nodeList (FTx FT.empty)
 where
   FTx ftFT = insFT total nodeList (FTx FT.empty)
   nodeList = randomRs (from,to) (mkStdGen seed)
   total    = if from <= to
              then (to - from) + 1
              else (from - to) + 1 

insFT :: (Num a,Ord a)=> a -> [a] -> FTx a -> FTx a
insFT _ [] ft = ft
insFT 0 _  ft = ft 
insFT n (x:xs) ft@(FTx tree) 
  | S.member x (measure tree) = insFT n     xs ft 
  | otherwise                 = insFT (n-1) xs (insert x ft)
 where
   insert :: (Num a,Ord a) => a -> FTx a -> FTx a
   insert x (FTx tree) = FTx $ (Elem x <| tree ) 


-- | EXAMPLE 
f0, f15, f15sing :: ForestEF Int
f0      = singletons (1,15)    19 emptyForest
f15     = rndForest  (1,15) 14 19 f0
f15sing = dynOpsNodes (repeat cut) (rndList 60 (1,15) 19) f15  



