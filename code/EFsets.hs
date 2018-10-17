{-#  LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module EFsets where 

import Data.FingerTree
import qualified Data.FingerTree as FT
import EtRt
import qualified Data.Set as S
import Data.Maybe (fromJust) 
import GHC.Exts (IsList(..))
import Data.Monoid ((<>))

-- ************************************************************
--    FT (Set (a,a))  (a,a)   

type TreeEF   a = FingerTree (S.Set (a,a)) (a,a)
type ForestEF a = FingerTree (S.Set (a,a)) (TreeEF a) 

emptyForest :: Ord a => ForestEF a  
emptyForest  = FT.empty 

emptyTree :: Ord a => TreeEF a 
emptyTree  = FT.empty 

instance (Ord a) => Measured (S.Set (a,a)) (a,a) where 
   measure (x,y) = S.insert (x, y) S.empty 

-- ----------------------------------------------------------
--           ROOT of tree 

root :: Ord a => TreeEF a -> Maybe a  
root tree = case viewl tree of
  EmptyL   -> Nothing
  x :< _   -> Just $ fst x

-- ----------------------------------------------------------
--           REroot of tree 

reroot :: Ord a => TreeEF a -> a -> TreeEF a 
reroot tree vertex = case (FT.search pred tree) of
   Position left _ right -> root <| (right >< left)
   _                     -> tree
 where root          = (vertex,vertex)
       pred before _ = (S.member root) before

-- ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
-- 
--            CONNECTED ( u, v ) in same tree ? IN WHICH tour ?

searchFor :: Ord a => a -> ForestEF a -> Maybe (TreeEF a, a) 
searchFor v f = 
 case FT.search pred f of 
  Position _ tree _ -> Just (tree, fromJust $ root tree) 
  _                 -> Nothing
 where
   pred before _ = (S.member (v,v)) before 

type PairTreeVertex a = (TreeEF a, a, TreeEF a, a) 

connected :: Ord a => a -> a -> ForestEF a -> (Bool, Maybe (PairTreeVertex a)) 
connected x y f = 
 case (searchFor x f, searchFor y f) of 
  (Nothing          , _           ) -> (False, Nothing) 
  (_                , Nothing     ) -> (False, Nothing) 
  (Just (tx,rx)     , Just (ty,ry)) -> if rx == ry 
                                   then (True,  Just(tx,rx,tx,rx))  
                                   else (False, Just(tx,rx,ty,ry))  
-- ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
--
--                           L I N K  (for trees and forest)
--

linkTree :: Ord a => a -> TreeEF a -> a -> TreeEF a -> Maybe (TreeEF a) 
linkTree u tu v tv = case (pairIn (u,u) tu, pairIn (v,v) tv) of
  (False, _    ) -> Nothing
  (_    , False) -> Nothing 
  (True , True ) -> Just $
    let from = reroot tu u
        (Position left _ right) = FT.search pred tv
    in  ((left |> (v,v)) |> (v,u)) >< from >< ((u,v) <| right)
 where
   pred before _ = (S.member (v,v)) before

link :: Ord a => a -> a -> ForestEF a -> ForestEF a 
link x y f 
  | x == y    = f  -- FURTHER MSG management
  | otherwise = 
     case connected x y f of 
      (False, Just (tx,rx,ty,ry)) -> case (linkTree x tx y ty) of
         Nothing     -> f
         Just result -> linkAll result 
      _                           -> f 
 where 
    Position lf' _ rf' = FT.search predX f 
    Position lf  _ rf  = FT.search predY (lf' >< rf') 
    linkAll tree    = tree <| (lf >< rf)
    predX before _ = (S.member (x,x)) before 
    predY before _ = (S.member (y,y)) before 

-- ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
--
--                             C U T   (for trees and forest)
--

cutTree :: Ord a => a -> a -> TreeEF a -> Maybe (TreeEF a,TreeEF a) 
cutTree u v tree = case FT.search predUV tree of
 Position left _ right ->
   case (FT.search predVU left ) of
      Position leftL _ rightL ->           -- (v,u) is on the left 
        Just (rightL, leftL >< right)
      _              ->                    -- (v,u) is on the right
        case (FT.search predVU right) of
          Position leftR _ rightR ->
            Just (leftR, left >< rightR)
          _ -> Nothing -- >>>> BAD Formed tree since (v,u) is missing 
 _  -> Nothing  -- >>>>>>>>> BAD Formed tree since (u,v) is missing     
 where
   predUV before _ = (S.member (u,v)) before 
   predVU before _ = (S.member (v,u)) before 


cut :: Ord a => a -> a -> ForestEF a -> ForestEF a 
cut x y f  
 | x == y    = f  -- further notice about NOT cut computed 
 | otherwise = 
    case connected x y f of 
      (True, Just (tx,_,_,_)) -> case (cutTree x y tx) of
        Nothing     -> f 
        Just result -> buildForest result  
      _                       -> f -- further notice NOT cut ... 
 where 
    buildForest (t2,t3) = t2 <| (t3 <| (lf >< rf)) 
    Position lf _ rf = FT.search pred f
    pred before _    = (S.member (x,x)) before


-- ooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo
--
--            HELPER functions  (for trees and forest)
--

pairIn :: (Measured (S.Set a) a, Ord a)
       => a -> FingerTree (S.Set a) a
       -> Bool
pairIn p monFT = case (FT.search pred monFT) of
  Position _ _ _ -> True 
  _              -> False
 where
   pred before _ = (S.member p) before 


nodeInF :: Ord a => (a,a) -> ForestEF a -> Bool
nodeInF x monFT = case (FT.search pred monFT) of
  Position _ _ _ -> True
  _              -> False
 where pred before _ = (S.member x) before 

toListFT :: (Ord a, Measured v a) => FingerTree v a -> [a]
toListFT ft = case (viewl ft) of
  EmptyL    -> []
  x :< rest -> x : toListFT rest


prtTOUR :: (Ord a, Show a) => TreeEF a -> IO () 
prtTOUR  = prtTree . et2rt . toListFT

prtMTOUR :: (Ord a, Show a) => Maybe (TreeEF a) -> IO () 
prtMTOUR  met = case met of
  Nothing -> putStrLn "\n No tree generated \n"
  Just et -> prtTree . et2rt . toListFT $ et 

prtFOREST :: (Ord a, Show a) => ForestEF a -> IO () 
prtFOREST f = ( prtForest . ets2rf ) ( forest f )
 where
   forest f = case viewl f of
     EmptyL   -> []
     x :<  xs -> toListFT x : forest xs 

-- -------------------------------------------------------------------------  
--
--    SOME TREE AND FOREST EXAMPLES 

ft1,ft2,s8,s9 :: TreeEF Int
ft1 = foldr (<|) emptyTree [(1,1),(1,2),(2,2),(2,1),(1,3),(3,3),(3,1),(1,4),(4,4),(4,1)] 
ft2 = foldr (<|) emptyTree [(5,5),(5,6),(6,6),(6,7),(7,7),(7,6),(6,5)] 
s8 = (8,8) <| emptyTree ; s9 = (9,9) <| emptyTree

forest1,forest2 :: ForestEF Int
forest1 =  (ft1 <| FT.empty) |> ft2  

forest2 = foldr (<|) FT.empty [s9,ft1,ft2,s8]

