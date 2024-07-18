import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char (digitToInt)
import Util

type Grid = M.Map [Int] Int

-- preprocess

parse :: String -> Grid
parse input = M.fromList kv
  where arr = map (map digitToInt) $ lines input
        kv = [([x,y],v) | (x,row) <- zip [0..] arr,
                          (y,v) <- zip [0..] row]

shift :: Int -> Int -> Int
shift dx x = if x' == 0 then 9 else x'
  where x' = (x + dx) `mod` 9

expand :: Grid -> Grid
expand grid = M.unions $ map getShifted deltas
  where [n,m] = map succ . fst $ M.findMax grid
        deltas = [[dx,dy] | dx <- [0..4], dy <- [0..4]]
        getShifted [dx,dy] = M.map (shift (dx+dy))
          $ M.mapKeys (zipWith (+) [dx*n,dy*m]) grid

-- dijkstra with Set as PQ

getNbrs :: [Int] -> [[Int]]
getNbrs pt = map (zipWith (+) pt) [[1,0],[-1,0],[0,1],[0,-1]]

getDist :: Grid -> Grid -> Int -> [Int] -> Int
getDist grid dist d node = min old new
  where old = M.findWithDefault maxBound node dist
        new = grid M.! node + d

dijkstra :: Grid -> Grid -> S.Set [Int] -> S.Set (Int, [Int]) -> Int
dijkstra grid dist visited pq
  | current == fst (M.findMax grid) = d
  | current `S.member` visited = dijkstra grid dist visited pq'
  | otherwise =
      let nbrs = filter (`M.member` grid) $ getNbrs current
          nbrDists = map (getDist grid dist d) nbrs
          dist' = foldr (uncurry M.insert) dist $ zip nbrs nbrDists
          visited' = S.insert current visited
          pq'' = foldr S.insert pq' $ zip nbrDists nbrs
      in dijkstra grid dist' visited' pq''
  where
    ((d, current), pq') = S.deleteFindMin pq


solve grid = dijkstra grid M.empty S.empty $ S.singleton (0, [0,0])

main = interact $ Util.print . solve . expand . parse
