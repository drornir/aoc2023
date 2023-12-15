{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use zipWith" #-}
module Main (main) where

import Data.Array (Array, array, bounds, indices, listArray, (!))
import Data.Char (isDigit)
import Data.List (foldl')
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Debug.Trace as Trace

main :: IO ()
main =
  do
    file <- readFile "inputs/day3.txt"
    let sch = fileToSchematic file
    print $ part2 sch

type Schematic = Array (Int, Int) Char

charAt :: Schematic -> (Int, Int) -> Char
charAt sch (i, j)
  | i < 0 || j < 0 = '.'
  | i > width || j > height = '.'
  | otherwise = sch ! (i, j)
  where
    (width, height) = snd $ bounds sch

fileToSchematic :: String -> Schematic
fileToSchematic file =
  let ls = lines file
      vertLen = length ls
      horzLen = length $ head ls
      schBounds = ((0, 0), (vertLen - 1, horzLen - 1))
      ((i0, j0), (i', j')) = schBounds
   in array schBounds [((i, j), c) | (i, l) <- zip [i0 .. i'] ls, (j, c) <- zip [j0 .. j'] l]

--
type Visited = Map.Map (Int, Int) ()

part2 :: Schematic -> Integer
part2 sch =
  let res = foldl' f 0 $ indices sch
   in res
  where
    -- Integer is sum of multiplications of numbers around cogs
    f :: Integer -> (Int, Int) -> Integer
    f acc (i, j)
      | let c = (sch ! (i, j)) in c == '*' =
          let sm = Trace.traceShow ("visitStar", (i, j)) visitStar sch (i, j)
           in (acc + sm)
      | otherwise = acc

visitStar :: Schematic -> (Int, Int) -> Integer
visitStar sch (i, j) =
  let c = sch ! (i, j)
      numbersAround = findNumbersAround sch (i, j)
   in Trace.traceShow
        ("numbersAround", numbersAround)
        ( if length numbersAround == 2
            then foldl' (*) 1 numbersAround
            else 0
        )

findNumbersAround :: Schematic -> (Int, Int) -> [Integer]
findNumbersAround sch (i, j) =
  let surrounding =
        [ (i + is, j + js)
          | (is, js) <- [(x, y) | x <- [-1, 0, 1], y <- [-1, 0, 1]],
            (is, js) /= (0, 0)
        ]
   in visit surrounding
  where
    visit :: [(Int, Int)] -> [Integer]
    visit [] = []
    visit (idx : idxs) = visit' Map.empty [] (idx : idxs)
    visit' :: Visited -> [Integer] -> [(Int, Int)] -> [Integer]
    visit' visited acc [] = acc
    visit' visited acc ((i, j) : idxs) =
      let visited' = Map.insert (i, j) () visited
          isNotVisited = Maybe.isNothing (visited Map.!? (i, j))
       in if isNotVisited
            then
              let c = Trace.traceShow ("->> visiting new idx", (i, j), charAt sch (i, j)) charAt sch (i, j)
                  (visited'', num) = if isDigit c then Trace.traceShow ("discovering number", c, (i, j)) discoverNumber visited' (i, j) else (visited', Maybe.Nothing)
                  acc' = if Maybe.isJust num then Maybe.fromJust num : acc else acc
               in visit' visited'' acc' idxs
            else Trace.traceShow ("->> already visited idx", (i, j), charAt sch (i, j), idxs) visit' visited' acc idxs
      where
        discoverNumber :: Visited -> (Int, Int) -> (Visited, Maybe.Maybe Integer)
        discoverNumber visited (i, j) =
          let (rightVisited, rightSide) = discoverRight visited "" (i, j)
              (leftVisited, leftSide) = discoverLeft visited "" (i, j)
              visited' = Trace.traceShow ("left", leftSide, "right", rightSide) Map.union rightVisited $ Map.union leftVisited $ Map.insert (i, j) () visited
           in (visited', Maybe.Just (read (leftSide ++ [charAt sch (i, j)] ++ rightSide) :: Integer))
        discoverRight visited n (i, j)
          | isDigit $ charAt sch (i, j + 1) = 
              let idx = (i, j + 1)
                  visited' = Map.insert idx () visited
                  n' =  n ++ [charAt sch idx]
               in discoverRight visited' n' idx
          | otherwise = (Map.insert (i, j) () visited, n)
        discoverLeft visited n (i, j)
          | isDigit $ charAt sch (i, j - 1) =
              let idx = (i, j - 1)
                  visited' = Map.insert idx () visited
                  n' = (charAt sch idx : n)
               in discoverLeft visited' n' idx
          | otherwise = (Map.insert (i, j) () visited, n)

-- 533775
part1 :: Schematic -> Integer
part1 sch =
  let (res, _, _) = foldl' f (0, 0, False) $ indices sch
   in res
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
   in Trace.traceShow (i, j, surrounding, surrounding', map f surrounding') any f surrounding'
  where
    f :: (Int, Int) -> Bool
    f (i, j) =
      let c = sch ! (i, j)
       in c /= '.' && not (isDigit c)
