{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use zipWith" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}
{-# HLINT ignore "Use bimap" #-}
module Main (main) where

import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Debug.Trace as Trace

main :: IO ()
main =
  do
    file <- readFile "inputs/day5_ex.txt"
    print $ part1 $ parseFile file

type Mapping = Map.Map Integer Integer

type Mappings = Map.Map String Mapping

type Seeds = [Integer]

part1 :: (Seeds, Mappings) -> Integer
part1 (seeds, mappings) = 0

parseFile :: String -> (Seeds, Mappings)
parseFile file =
  let (firstLine : lines) = List.lines file
      seeds = parseSeeds firstLine
      mappings = parseMappings lines
   in (seeds, mappings)

parseSeeds :: String -> Seeds
parseSeeds line =
  let strs = Text.split (== ' ') $ Text.pack line
      tl = List.tail strs
   in map (\str -> read $ Text.unpack str :: Integer) tl

parseMappings :: [String] -> Mappings
parseMappings lines =
  let (mappings, _) = List.foldl' pm (Map.empty, Text.empty) lines
   in mappings
  where
    pm :: (Mappings, Text.Text) -> String -> (Mappings, Text.Text)
    pm (maps, currentMap) line
      | line' == Text.empty = (maps, currentMap) -- skip empty lines
      | Text.pack " map:" `Text.isSuffixOf` line' =
          (maps, Text.takeWhile (/= ' ') line')
      -- 3 numbers per row
      | otherwise =
          let (l, r, delta) = parse3Nums line'
              oldMappingMaybe = (maps Map.!? Text.unpack currentMap)
              oldMapping = Maybe.fromMaybe Map.empty oldMappingMaybe
              newKVs = Map.fromList $ List.zip [r .. r + delta - 1] [l .. l + delta - 1]
              newMapping = Map.union oldMapping newKVs
              maps' = Map.insert (Text.unpack currentMap) newMapping maps
           in (maps', currentMap)
      where
        line' = Text.strip $ Text.pack line
        parse3Nums :: Text.Text -> (Integer, Integer, Integer)
        parse3Nums line =
          let numStr = Text.split Char.isSpace line
              nums = map (\str -> read $ Text.unpack str :: Integer) numStr
              (a : b : c : _) = nums
           in (a, b, c)