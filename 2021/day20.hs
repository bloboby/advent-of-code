import qualified Data.Map as M
import Data.List.Split (splitOn)
import Util

type Key = M.Map Int Char
type Grid = M.Map [Int] Char

nbrs [x,y] = [[a,b] | a <- [x-1..x+1], b <- [y-1..y+1]]

binToDec = foldl (\acc c -> if c == '.' then acc*2 else acc*2+1) 0

preprocess :: String -> (Key, Grid)
preprocess s = (key, M.fromList grid)
  where [a,b] = splitOn "\n\n" s
        key = M.fromList $ zip [0..] a
        grid = [([x,y],v) | (x,row) <- zip [0..] $ lines b,
                            (y,v) <- zip [0..] $ row]

expand :: Grid -> Char -> Grid
expand grid background = M.union grid grid'
  where foreground = M.keys $ M.filter (/=background) grid
        allNbrs = concat $ map nbrs foreground
        grid' = M.fromList . zip allNbrs $ repeat background

enhance :: Key -> (Grid, Char) -> (Grid, Char)
enhance key (grid, bg) = (grid', bg')
  where getChar k _ =
          let find nbr = M.findWithDefault bg nbr grid
              pixels = map find $ nbrs k
          in key M.! (binToDec pixels)
        grid' = M.mapWithKey getChar $ expand grid bg
        bg' = if (bg == '.') then (key M.! 0) else (key M.! 511)

solve (key, grid) = length . M.filter (=='#') $ image
  where image = fst . (!!50) . iterate (enhance key) $ (grid, '.')

main = interact $ Util.print . solve . preprocess
