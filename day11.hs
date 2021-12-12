import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char (digitToInt)
import Util

type Grid = M.Map [Int] Int
type Points = S.Set [Int]

preprocess :: String -> Grid
preprocess input = M.fromList kv
  where arr = map (map digitToInt) $ lines input
        kv = [([x,y],v) | (x,row) <- zip [0..] arr,
                          (y,v) <- zip [0..] row]

flash :: (Grid, Points) -> [Int] -> (Grid, Points)
flash (grid, flashed) k 
  | k `S.member` flashed || k `M.notMember` grid = (grid, flashed)
  | grid' M.! k < 10 = (grid', flashed)
  | otherwise = foldl flash state nbrs
  where grid' = M.insertWith (+) k 1 grid
        state = (M.insert k 0 grid, S.insert k flashed)
        nbrs = map (zipWith (+) k) [[1,0],[-1,0],[0,1],[0,-1],
                                    [1,1],[1,-1],[-1,1],[-1,-1]]

evolve n grid = if length g == length f then n else evolve (n+1) g
  where grid' = M.map succ grid
        toFlash = M.filter (>9) grid'
        (g, f) = foldl flash (grid', S.empty) $ M.keys toFlash

main = interact $ Util.print . evolve 1 . preprocess
