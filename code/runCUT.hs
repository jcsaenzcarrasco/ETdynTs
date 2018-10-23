
module Main where

import RndDynTs
import EFsets
import Data.FingerTree
import System.Environment 

main = do
 let notice = "\nSyntax: ./run <#Operations> <ForestSize(#nodes)> <TreeSize(#edges)> <RndSeed> <DisplayOpt(-n:non visual | -p:print forest)>\n"
 putStrLn notice
 args <- getArgs
 let no = read (args !! 0) :: Int -- number of ops
 let fs = read (args !! 1) :: Int -- size of forest
 let ts = read (args !! 2) :: Int -- size of trees
 let rs = read (args !! 3) :: Int -- random seed
 let fl = args !! 4 -- "-n" just head and last pairs; "-p" tree printed 
 let ops = (repeat cut) 
 let nds = rndList (2*no) (1,fs) rs
 let initF = rndForest    (1,fs) ts rs emptyForest
 let forest = dynOpsNodes ops nds initF
 let (f' :< _) = viewl forest
 let (f  :< _) = viewl f' 
 let (_ :> l') = viewr forest
 let (_ :> l ) = viewr l' 
 if fl == "-n" then
    putStrLn $ "First: " ++ show f ++ "; Last: " ++ show l 
 else if fl == "-p" then
        prtFOREST forest
      else
        putStrLn notice

