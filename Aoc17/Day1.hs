{-# OPTIONS_GHC -O2 #-}

module Aoc17.Day1
( day1Captcha
, day1Captcha2
) where

_captcha :: [Int] -> Int -> Int -> Int
_captcha []      _   s = s
_captcha (hd:ls) prv n = _captcha ls hd (n + (if hd == prv then hd else 0))

day1Captcha :: [Int] -> Int
day1Captcha l = _captcha l (last l) 0

_captcha2 :: [Int] -> Int -> Int -> Int
_captcha2 _ 0     s = s
_captcha2 l count s = _captcha2 l (count - 1) (s + new)
  where new = if n1 == n2 then n1 else 0
        n1  = l !! (count `mod` length l)
        n2  = l !! ((count + length l `div` 2) `mod` length l)

day1Captcha2 :: [Int] -> Int
day1Captcha2 l = _captcha2 l (length l) 0
