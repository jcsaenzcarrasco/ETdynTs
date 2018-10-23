
module Main where

import RndDynTs
import EFsets
import Data.FingerTree
import System.Environment 

main = do
 let notice = "\nSyntax: ./run <#Operations> <ForestSize(#nodes)> <TreeSize(#edges)> <RndSeed> \n"
            ++"<DisplayOpt(-n:first and last pairs | -p:print forest | -l:list of results | -r:list of random nodes | -s:total Bool values)>\n"
 putStrLn notice
 args <- getArgs
 let no = read (args !! 0) :: Int -- number of ops
 let fs = read (args !! 1) :: Int -- size of forest
 let ts = read (args !! 2) :: Int -- size of trees
 let rs = read (args !! 3) :: Int -- random seed
 let fl = args !! 4 -- "-n" just head and last pairs; "-p" tree printed 
 let nds = rndList (2*no) (1,fs) rs
 let initF = rndForest    (1,fs) ts rs emptyForest
 let (connList,forest) = connX nds initF
 let (f' :< _)  = viewl forest
 let (f  :< _)  = viewl f' 
 let (_ :> l')  = viewr forest
 let (_ :> l )  = viewr l'
 let (totalT,totalF) = countTF connList
 case fl of
   "-n" -> putStrLn $ "First: " ++ show f ++ "; Last: " ++ show l
   "-p" -> prtFOREST forest
   "-l" -> putStrLn $ show connList
   "-r" -> putStrLn $ show nds
   "-s" -> putStrLn $ "True: "++show totalT++", False: "++show totalF++"\n"
   _    -> putStrLn notice


countTF []   = (0,0)
countTF xs   = (totT,totF)
  where
    totT = length $ filter (\x->x==True) xs
    totF = length $ filter (\x->x==False) xs
