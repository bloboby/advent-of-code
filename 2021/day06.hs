import qualified Data.Map as M
import Data.List.Split (splitOn)
import Util

parse :: String -> [Int]
parse input = M.elems $ foldr (M.adjust succ) counter list
  where counter = M.fromList . zip [0..8] $ repeat 0
        list = map read . splitOn "," . head . lines $ input

solve :: [Int] -> Int
solve = sum . (!! 256) . iterate evolve
  where evolve [a,b,c,d,e,f,g,h,i] = [b,c,d,e,f,g,h+a,i,a]

main = interact $ Util.print . solve . parse
