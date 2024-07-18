import Data.List.Split (splitOn)
import Util

parse :: String -> [Int]
parse line =
  let [[a,b],[c,d]] = map (splitOn "-") $ splitOn "," line
  in map read [a,b,c,d]

count1 [a,b,c,d]
  | a <= c && b >= d = 1
  | a >= c && b <= d = 1
  | otherwise = 0

count2 [a,b,c,d]
  | b < c || d < a = 0
  | otherwise = 1

main = interact $ Util.print . sum . map (count2 . parse) . lines
