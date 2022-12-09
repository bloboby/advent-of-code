import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char (digitToInt)
import Data.Map (Map)
import Data.Set (Set)

type Coord = (Int, Int)
type Grid = Map Coord Int

parseGrid :: [[Char]] -> Grid
parseGrid rows = M.fromList $
  [((i,j),x) | (i,row) <- zip [0..] rows,
               (j,x) <- zip [0..] $ map digitToInt row]

getDirs :: [String] -> [[Coord]]
getDirs rows =
  let (n, m) = (length rows, length $ head rows)
      (ns, ms) = ([0..n-1], [0..m-1])
      fwd = [[(i,j) | j <- ms] | i <- ns] ++ [[(i,j) | i <-ns] | j <- ms]
  in fwd ++ map reverse fwd

getVis :: Grid -> [Coord] -> Set Coord
getVis grid coords =
  let f (maxH, vis) coord = let h = grid M.! coord
                            in if h > maxH then (h, S.insert coord vis)
                               else (maxH, vis)
  in snd $ foldl f (-1, S.empty) coords

main = do
  contents <- getContents
  let rows = lines contents
      grid = parseGrid rows
      part1 = length . foldl1 S.union . map (getVis grid) $ getDirs rows
  -- let display = putStrLn . unlines . map show
  print part1
