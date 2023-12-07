{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use zipWith" #-}
module Main where

import Data.Array (Array, array, bounds, indices, listArray, (!))
import Data.Char (isDigit)
import Data.List (foldl')
import Debug.Trace (trace, traceShow)

type Schematic = Array (Int, Int) Char

main :: IO ()
main =
  do
    file <- readFile "inputs/day3.txt"
    let sch = fileToSchematic file
    print $ part1 sch

fileToSchematic :: String -> Schematic
fileToSchematic file =
  let ls = lines file
      vertLen = length ls
      horzLen = length $ head ls
      schBounds = ((0, 0), (vertLen - 1, horzLen - 1))
      ((i0, j0), (i', j')) = schBounds
   in array schBounds [((i, j), c) | (i, l) <- zip [i0 .. i'] ls, (j, c) <- zip [j0 .. j'] l]

part1 :: Schematic -> Integer
part1 sch =
  let (summ, _, _) = foldl' f (0, 0, False) $ indices sch
   in summ
  where
    f :: (Integer, Integer, Bool) -> (Int, Int) -> (Integer, Integer, Bool)
    f (acc, numSoFar, surrounded) (i, j)
      | isDigit (sch ! (i, j)) =
          let prevIsDigit = j > 0 && isDigit (sch ! (i, j - 1))
              nextIsDigit = j < snd (snd $ bounds sch) && isDigit (sch ! (i, j + 1))
              numSoFar' = numSoFar * 10 + (read [sch ! (i, j)] :: Integer)
           in if nextIsDigit
                then (acc, numSoFar', surrounded || hasSurroundingSpecial sch (i, j))
                else
                  if surrounded || hasSurroundingSpecial sch (i, j)
                    then (acc + numSoFar', 0, False)
                    else (acc, 0, False)
      | otherwise = (acc, 0, False)

hasSurroundingSpecial :: Schematic -> (Int, Int) -> Bool
hasSurroundingSpecial sch (i, j) =
  let (maxI, maxJ) = snd $ bounds sch
      surrounding =
        [ (i + is, j + js)
          | (is, js) <- [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1]],
            is /= 0 || js /= 0
        ]
      surrounding' =
        [ (i', j')
          | (i', j') <- surrounding,
            i' >= 0 && i' <= maxI && j' >= 0 && j' <= maxJ
        ]
   in traceShow (i, j, surrounding, surrounding', (map f surrounding')) any f surrounding'
  where
    f :: (Int, Int) -> Bool
    f (i, j) =
      let c = sch ! (i, j)
       in c /= '.' && not (isDigit c)
