{-# OPTIONS_GHC -O2 #-}

module Aoc17.Day4
( day4Step1
, day4Step2
) where

import Data.List

phraseAllUnique :: [String] -> Bool
phraseAllUnique s = length s == length (nub s)

noAnagrams :: [String] -> Bool
noAnagrams []      = True
noAnagrams [_]     = True
noAnagrams (w:phr) = null (permutations w `intersect` phr) && noAnagrams phr

countBool :: [Bool] -> Int
countBool = sum . map fromEnum

day4Step1 :: [String] -> Int
day4Step1 = countBool . map (phraseAllUnique . words)

day4Step2 :: [String] -> Int
day4Step2 = countBool . map (noAnagrams . words)
