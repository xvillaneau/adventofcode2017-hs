{-# OPTIONS_GHC -O2 #-}

module Aoc17.Tools
( readDigits
, readNumberGrid
) where

import Data.Char

readDigits :: String -> [Int]
readDigits = map (\ c -> read [c] :: Int) . filter isDigit

readNumberGrid :: String -> [[Int]]
readNumberGrid = map (map read . words) . lines
