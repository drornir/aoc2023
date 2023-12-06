module Main where

import Data.List (isPrefixOf)
import Data.Text (Text, breakOn, drop, pack, split, strip, unpack)
import System.IO (readFile)
import Prelude hiding (drop)

main :: IO ()
main = do
  file <- readFile "inputs/day2.txt"
  print $ part1 file

data Draw = Draw
  { green :: Integer,
    red :: Integer,
    blue :: Integer
  }
  deriving (Show)

type MaxDraw = Draw

data Game = Game
  { _id :: Integer,
    draws :: [Draw]
  }
  deriving (Show)

part1 file =
  let ls = lines file
      games = map toGame ls
      maxDraw = (Draw {red = 12, green = 13, blue = 14})
   in sum $ map (idsOfPossibleGame maxDraw) games

idsOfPossibleGame :: MaxDraw -> Game -> Integer
idsOfPossibleGame md g =
  if all (isPossibleDraw md) (draws g)
    then _id g
    else 0

isPossibleDraw :: MaxDraw -> Draw -> Bool
isPossibleDraw md dr =
  (green dr <= green md)
    && (red dr <= red md)
    && (blue dr <= blue md)

toGame :: String -> Game
toGame l =
  let (numStr, l') = breakOn (pack ":") $ drop (length "Game ") (pack l)
      num = read (unpack numStr) :: Integer
   in Game {_id = num, draws = toDraws $ drop (length ": ") l'}

toDraws :: Text -> [Draw]
toDraws l = map toDraw $ split (== ';') l

toDraw :: Text -> Draw
toDraw l = foldl fillDraw (Draw 0 0 0) $ split (== ',') l

fillDraw :: Draw -> Text -> Draw
fillDraw d l =
  let (numS : name : _) = split (== ' ') $ strip l
      num = read $ unpack numS :: Integer
   in case unpack name of
        "green" -> d {green = num}
        "red" -> d {red = num}
        "blue" -> d {blue = num}
        _ -> d