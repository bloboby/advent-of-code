import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import Data.Char (ord)
import Data.Map (Map)
import Data.Set (Set)
import Data.Sequence (Seq(..), (><))

type Grid = Map [Int] Char

parseGrid :: String -> Grid
parseGrid contents = M.fromList $
  [([i,j],x) | (i, row) <- zip [0..] $ lines contents,
               (j, x) <- zip [0..] $ row]

getNbrs pt = map (zipWith (+) pt) [[1,0],[-1,0],[0,1],[0,-1]]

elev c = case c of
  'S' -> ord 'a'
  'E' -> ord 'z'
  _   -> ord c

isValid grid v pt' =
  if not $ pt' `M.member` grid then False
  else let v' = grid M.! pt' in elev v' - elev v <= 1

bfs :: Grid -> Set [Int] -> Seq ([Int], Int) -> Int
bfs grid vis ((pt,dist):<|q)
  | v == 'E' = dist
  | pt `S.member` vis = bfs grid vis q
  | otherwise =
      let vis' = S.insert pt vis
          nbrs = filter (isValid grid v) $ getNbrs pt
          q' = q >< (Seq.fromList . zip nbrs $ repeat (dist+1))
      in bfs grid vis' q'
  where v = grid M.! pt

main = do
  contents <- getContents
  let grid = parseGrid contents
      start = M.keys . M.filter (`elem` "aS") $ grid
      ans = bfs grid S.empty . Seq.fromList . zip start $ repeat 0
  print ans
