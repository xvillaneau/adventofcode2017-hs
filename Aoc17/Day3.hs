
module Aoc17.Day3
( day3Step1
, day3Step2
) where

import qualified Data.Map as Map
import Data.Maybe(fromMaybe)

type Pos = (Int, Int)

rotate90 :: Int -> Pos -> Pos
rotate90 0 v     = v
rotate90 n (x,y) =
  if n > 0
    then rotate90 (n - 1) (-y, x)
    else rotate90 (n + 1) (y, -x)

layer :: Int -> Int
layer n = div (1 + floor (sqrt $ fromIntegral (n - 1) :: Float)) 2

position :: Int -> Pos
position 1 = (0, 0)
position n =
  let l = layer n
      side_len = 2 * l
      start = (2 * l - 1) ^ (2 :: Int)
      (side, side_pos) = divMod (n - start) side_len
  in rotate90 side (l, side_pos - l)

manhattanDist :: Pos -> Int
manhattanDist (x, y) = abs x + abs y

neighbors :: Pos -> [Pos]
neighbors (x, y) = [(x+a, y+b) | a <- l, b <- l] where l = [-1, 0, 1]

uzumaki :: Int -> Int -> Map.Map Pos Int -> Int
uzumaki n i m =
  if val > n
    then val
    else uzumaki n (i + 1) (Map.insert pos val m)
  where
    pos = position i
    val = sum $ map (fromMaybe 0 . flip Map.lookup m) $ neighbors pos

day3Step1 :: Int -> Int
day3Step1 = manhattanDist . position

day3Step2 :: Int -> Int
day3Step2 n = uzumaki n 2 (Map.fromList [((0, 0), 1)])
