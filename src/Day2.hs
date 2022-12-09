module Main where

import Control.Category ((>>>))
import Data.Char (ord)

parse :: Char -> Int
parse = \case
  'A' -> 0
  'B' -> 2
  'C' -> 1
  'X' -> 0
  'Y' -> 2
  'Z' -> 1
  _ -> error "Unexpected"

score :: Int -> Int
score = \case
  0 -> 1
  2 -> 2
  1 -> 3
  _ -> error "Unexpected"

part1 :: [Char] -> Int
part1 = map parse >>> \ ~[om, mm] -> score mm + 3 * [2, 0, 1, 2, 0] !! (om - mm + 2)

part2 :: [Char] -> Int
part2 = (\ ~[m, r] -> (parse m, ord r - 88)) >>> (\(m, r) -> score ((m - r + 4) `rem` 3) + r * 3)

main :: IO ()
main = do
  input <- map (\moves -> [head moves, last moves]) . lines <$> getContents
  print $ map (sum . flip map input) [part1, part2]
