{-# OPTIONS_GHC -O2 #-}

import System.IO

import qualified Aoc17.Tools as Tools
import Aoc17.Day1
import Aoc17.Day2
import Aoc17.Day3
import Aoc17.Day4

dayFromFile :: Show b => Int -> (String -> a) -> (a -> b) -> (a -> b) -> IO ()
dayFromFile n reader step1 step2 = do
  let filename = "data/day" ++ show n ++ ".txt"
  handle <- openFile filename ReadMode
  input <- reader <$> hGetContents handle
  dayFromData n input step1 step2
  hClose handle

dayFromData :: Show b => Int -> a -> (a -> b) -> (a -> b) -> IO ()
dayFromData n input step1 step2 = do
  putStrLn $ "Day " ++ show n ++ ":"
  putStrLn $ " " ++ (show . step1) input
  putStrLn $ " " ++ (show . step2) input
  putStrLn ""

main :: IO()
main = do
  dayFromFile 1 Tools.readDigits day1Captcha day1Captcha2
  dayFromFile 2 Tools.readNumberGrid day2SubChecksum day2DivChecksum
  dayFromData 3 368078 day3Step1 day3Step2
  dayFromFile 4 lines day4Step1 day4Step2
