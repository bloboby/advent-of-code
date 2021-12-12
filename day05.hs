import qualified Data.Map as M
import Data.List.Split (splitOn)
import Util

type Counts = M.Map (Int, Int) Int

parse :: String -> [Int]
parse s = map read . concat . map (splitOn ",") $ [a,b]
  where [a, _, b] = words s

insertLn :: [Int] -> Counts -> Counts
insertLn [x1, y1, x2, y2] counts = foldr insertPt counts pts
  where insertPt pt = M.insertWith (+) pt 1
        [dx, dy] = map signum [x2 - x1, y2 - y1]
        pts = zip [x1, x1+dx .. x2] [y1, y1+dy .. y2]

solve :: [[Int]] -> Int
solve = length . M.filter (>1) . foldr insertLn M.empty

main = interact $ Util.print . solve . map parse . lines
