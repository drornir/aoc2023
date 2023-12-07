module Main where

main :: IO ()
main =
  do
    file <- readFile "inputs/day3.txt"
    let ls = lines file
    print ls