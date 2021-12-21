module Main where

import Control.Monad (replicateM)
import System.Random (randomRIO)
import Data.Time

main :: IO ()
main = do
  start <- getCurrentTime 
  randomList <- randomInts 500000 (0,9)
  --print randomList DO NOT UNTAB THIS 
  let s = sumRandomList (toMap 1 randomList)
  print s
  stop <- getCurrentTime
  print $ diffUTCTime stop start 
  

sumRandomList :: [Int] -> Int
sumRandomList list = sum list

toMap :: Int -> [Int] -> [Int]
toMap x [] = []
toMap x (y : ys) = y + x : toMap x (ys)

randomInts :: Int -> (Int,Int) -> IO [Int]
randomInts len bounds = replicateM len $ randomRIO bounds