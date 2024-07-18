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

-- part 1

generateLines :: [String] -> [[Coord]]
generateLines rows =
  let (n, m) = (length rows, length $ head rows)
      (ns, ms) = ([0..n-1], [0..m-1])
      fwd = [[(i,j) | j <- ms] | i <- ns] ++ [[(i,j) | i <-ns] | j <- ms]
  in fwd ++ map reverse fwd

foldVis grid (maxH, vis) coord =
  let h = grid M.! coord
  in if h > maxH then (h, S.insert coord vis) else (maxH, vis)

getVis :: Grid -> [Coord] -> Set Coord
getVis grid coords = snd $ foldl (foldVis grid) (-1, S.empty) coords

-- part 2

generateViews :: Coord -> [[Coord]]
generateViews (x,y) =
  [zip [x+1..] (repeat y),
   zip [x-1,x-2..] (repeat y),
   zip (repeat x) [y+1..],
   zip (repeat x) [y-1,y-2..]]

getViewScore :: Grid -> Int -> [Coord] -> Int
getViewScore grid h (x:xs)
  | not $ M.member x grid = 0
  | grid M.! x >= h = 1
  | otherwise = 1 + getViewScore grid h xs

getScore :: Grid -> Coord -> Int
getScore grid coord =
  let h = grid M.! coord
  in product . map (getViewScore grid h) $ generateViews coord

main = do
  contents <- getContents
  let rows = lines contents
      grid = parseGrid rows
      part1 = length . foldl1 S.union . map (getVis grid) $ generateLines rows
      part2 = maximum . map (getScore grid) $ M.keys grid
  print (part1, part2)
