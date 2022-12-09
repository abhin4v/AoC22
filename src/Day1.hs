module Main where

import Data.List (sortOn)
import Data.List.Split (splitWhen)

import Data.Ord (Down (..))

part1 :: [String] -> Integer
part1 = maximum . map (sum . map read) . splitWhen (== "")

part2 :: [String] -> Integer
part2 = sum . take 3 . sortOn Down . map (sum . map read) . splitWhen (== "")

main :: IO ()
main = do
  input <- lines <$> getContents
  print $ part1 input
  print $ part2 input
