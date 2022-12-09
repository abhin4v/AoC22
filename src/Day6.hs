module Main where

import Data.List (find, nub, transpose)
import Data.Maybe (fromJust)

solve :: Int -> String -> Int
solve n input =
  (+ (n - 1))
    . fst
    . fromJust
    . find ((== n) . length . snd)
    . zip [1 ..]
    . map nub
    . transpose
    . map (`drop` input)
    $ [0 .. (n - 1)]

main :: IO ()
main = do
  input <- getContents
  print $ solve 4 input
  print $ solve 14 input
