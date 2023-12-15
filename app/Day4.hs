{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use zipWith" #-}
module Main (main) where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Text as Text
import qualified Debug.Trace as Trace

main :: IO ()
main =
  do
    file <- readFile "inputs/day4.txt"
    print $ part1 $ fileToCards file

data Card = Card
  { _num :: Integer,
    _wins :: [Integer],
    _have :: [Integer]
  }
  deriving (Show, Eq)

fileToCards :: String -> [Card]
fileToCards file =
  let lines = List.lines file
   in List.foldr f [] lines
  where
    f :: String -> [Card] -> [Card]
    f line acc = lineToCard line : acc

lineToCard :: String -> Card
lineToCard line =
  let l = Text.pack line
      (gamePart, rest) = Text.break (== ':') l
      n = read $ Text.unpack $ Text.dropWhile (\c -> Char.isAlpha c || Char.isSpace c) gamePart :: Integer
      (winPart, havePart) = Text.break (== '|') rest
      (winPart', havePart') = (Text.drop 1 winPart, Text.drop 1 havePart)
      parseListOfInts part = List.map (\w -> read (Text.unpack w) :: Integer) $ Text.words part
      wins = parseListOfInts winPart'
      have = parseListOfInts havePart'
   in Card {_num = n, _wins = wins, _have = have}

part1 :: [Card] -> Integer
part1 cards = sum $ map cardValue cards

cardValue :: Card -> Integer
cardValue card =
  let matches = List.foldl' f [] $ _wins card
   in ( if not (null matches)
          then 2 ^ (List.length matches - 1)
          else 0
      )
  where
    f :: [Integer] -> Integer -> [Integer]
    f acc winNum =
      if winNum `List.elem` _have card
        then winNum : acc
        else acc