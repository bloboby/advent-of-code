import Util

parse :: String -> (String, Int)
parse s = (a, read b) where [a,b] = words s

moveSubmarine :: [Int] -> (String, Int) -> [Int]
moveSubmarine [x,d,a] (cmd, n) = case cmd of
  "forward" -> [x+n, d+a*n, a]
  "down"    -> [x, d, a+n]
  "up"      -> [x, d, a-n]

solve :: [(String, Int)] -> Int
solve = product . take 2 . foldl moveSubmarine [0,0,0]

main = interact $ Util.print . solve . map parse . lines
