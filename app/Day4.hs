{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use zipWith" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Use bimap" #-}
module Main (main) where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Debug.Trace as Trace

main :: IO ()
main =
  do
    file <- readFile "inputs/day4_ex.txt"
    print $ part2 $ fileToCards file

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

findMatches :: Card -> [Integer]
findMatches card = List.foldl' f [] $ _wins card
  where
    f :: [Integer] -> Integer -> [Integer]
    f acc winNum =
      if winNum `List.elem` _have card
        then winNum : acc
        else acc

--
part2 :: [Card] -> Integer
part2 [] = 0
part2 (c : cx) =
  let n = countWinningCards (c, cx)
      rest = part2 cx
   in Trace.traceShow ("part2", _num c, n, rest) (n + rest)

part2' :: [Card] -> Integer
part2' [] = 0
part2' (c : cx) = countWinningCards (c, cx)

countWinningCards :: (Card, [Card]) -> Integer
countWinningCards (current, nextCards) =
  let matches = findMatches current
      winnings = length matches
      newCards = List.take winnings nextCards
      newCardsZippedWithRest = List.zip newCards $ map (\i -> List.drop i nextCards) [1 .. winnings]
      -- [ (c, cx) | c <- newCards, cx <- map (\i -> List.drop i nextCards) [1 .. 5]
      -- ]
      winningsFromWinnings = sum (map countWinningCards newCardsZippedWithRest)
   in Trace.traceShow
        ( "card",
          _num current,
          "winnings",
          winnings,
          "total",
          toInteger winnings + winningsFromWinnings,
          "newCards",
          map _num newCards,
          "restNum",
          map (\(c, cx) -> (_num c, map _num cx)) newCardsZippedWithRest
        )
        (toInteger winnings + winningsFromWinnings)

-- 21213
part1 :: [Card] -> Integer
part1 cards = sum $ map cardValue1 cards

cardValue1 :: Card -> Integer
cardValue1 card =
  let matches = findMatches card
   in if not (null matches)
        then 2 ^ (List.length matches - 1)
        else 0
