{-# OPTIONS_GHC -O2 #-}

module Aoc17.Day2
( day2DivChecksum
, day2SubChecksum
) where

import Data.List (tails)

combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [ [] ]
combinations n xs = [ y:ys | y:xs' <- tails xs,
                      ys <- combinations (n-1) xs']

pairs :: [a] -> [(a, a)]
pairs l = map (\(x:y:_) -> (x, y)) $ combinations 2 l

firstDividable :: [(Int, Int)] -> (Int, Int)
firstDividable [] = error "No dividable ints"
firstDividable l
  | a `mod` b == 0  = (a, b)
  | b `mod` a == 0  = (b, a)
  | otherwise       = firstDividable ln
  where (a,b):ln = l

lineValDiv :: [Int] -> Int
lineValDiv s = a `div` b
  where (a, b) = firstDividable $ pairs s

lineValSub :: [Int] -> Int
lineValSub s = maximum s - minimum s

day2DivChecksum :: [[Int]] -> Int
day2DivChecksum = sum . map lineValDiv

day2SubChecksum :: [[Int]] -> Int
day2SubChecksum = sum . map lineValSub
