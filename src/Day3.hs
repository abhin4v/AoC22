module Main where

import Control.Category ((>>>))
import Data.Char (ord)
import Data.List (intersect, nub)
import Data.List.Split (chunksOf)

prio :: Char -> Int
prio c = let o = ord c in if o >= 97 then o - 96 else o - 64 + 26

part1 :: [[Char]] -> Int
part1 = map (\l -> splitAt (length l `div` 2) l) >>> map (uncurry intersect >>> nub >>> head >>> prio) >>> sum

part2 :: [[Char]] -> Int
part2 = chunksOf 3 >>> map (foldl1 intersect >>> nub >>> head >>> prio) >>> sum

main :: IO ()
main = do
  input <- lines <$> getContents
  print $ part1 input
  print $ part2 input
