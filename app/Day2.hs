module Main where

import Data.List (foldl', isPrefixOf)
import Data.Text (Text, breakOn, drop, pack, split, strip, unpack)
import System.IO (readFile)
import Prelude hiding (drop)

main :: IO ()
main = do
  file <- readFile "inputs/day2.txt"
  let ls = lines file
      games = map toGame ls
  print $ part2 games

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

-- 70924
part2 :: [Game] -> Integer
part2 games =
  sum $ map (powerOfDraw . minDrawOfGame) games

powerOfDraw :: Draw -> Integer
powerOfDraw (Draw x y z) = x * y * z

minDrawOfGame :: Game -> Draw
minDrawOfGame g = foldl' maxOfEachColor (Draw 0 0 0) (draws g)

maxOfEachColor :: Draw -> Draw -> Draw
maxOfEachColor dmax d =
  Draw
    { green = max (green dmax) (green d),
      red = max (red dmax) (red d),
      blue = max (blue dmax) (blue d)
    }

-- 2771
part1 :: [Game] -> Integer
part1 games =
  let maxDraw = (Draw {red = 12, green = 13, blue = 14})
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

--
toGame :: String -> Game
toGame l =
  let (numStr, l') = breakOn (pack ":") $ drop (length "Game ") (pack l)
      num = read (unpack numStr) :: Integer
   in Game {_id = num, draws = toDraws $ drop (length ": ") l'}

toDraws :: Text -> [Draw]
toDraws l = map toDraw $ split (== ';') l

toDraw :: Text -> Draw
toDraw l = foldl' fillDraw (Draw 0 0 0) $ split (== ',') l

fillDraw :: Draw -> Text -> Draw
fillDraw d l =
  let (numS : name : _) = split (== ' ') $ strip l
      num = read $ unpack numS :: Integer
   in case unpack name of
        "green" -> d {green = num}
        "red" -> d {red = num}
        "blue" -> d {blue = num}
        _ -> d