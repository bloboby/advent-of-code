import Data.List.Split (chunksOf)
import Data.Map (Map)
import Data.Map qualified as M

type Coord = (Int, Int)

type Grid = Map Coord Char

type Loop = Map Coord Int

kSPipe :: Char = 'J' -- hardcoded

parseGrid :: [[Char]] -> (Coord, Grid)
parseGrid rows =
  let f = zip [0 ..]
      grid = [((i, j), x) | (i, row) <- f rows, (j, x) <- f row]
      [(start, _)] = filter (\(_, x) -> x == 'S') grid
      grid' = M.insert start kSPipe . M.fromList $ grid
   in (start, grid')

-- [n,e,s,w]
pipe :: Char -> [Bool]
pipe x = case x of
  '|' -> [True, False, True, False]
  '-' -> [False, True, False, True]
  'L' -> [True, True, False, False]
  'J' -> [True, False, False, True]
  '7' -> [False, False, True, True]
  'F' -> [False, True, True, False]
  '.' -> [False, False, False, False]
  'S' -> pipe kSPipe

nbrs :: Coord -> [Bool] -> [Coord]
nbrs (x, y) nesw =
  let f (dx, dy) = (x + dx, y + dy)
      dxy = map f [(-1, 0), (0, 1), (1, 0), (0, -1)]
   in map fst . filter snd $ zip dxy nesw

-- Traverses one-way only.
walk :: Grid -> Int -> Loop -> Coord -> Loop
walk grid v dist k
  | k `M.member` dist = dist
  | not (k `M.member` grid) = dist
  | otherwise =
      let ns = nbrs k . pipe $ grid M.! k
          dist' = M.insert k v dist
       in foldl (walk grid (v + 1)) dist' ns

inside :: Grid -> Loop -> Coord -> Int
inside grid loop (x, y)
  | (x, y) `M.member` loop = 0
  | otherwise =
      let line = map (,y) [0 .. x]
          pipes = map (grid M.!) $ filter (`M.member` loop) line
          turns = chunksOf 2 $ filter (`elem` "FLJ7") pipes
          t1 = length $ filter (== "FJ") turns
          t2 = length $ filter (== "7L") turns
          h = length $ filter (== '-') pipes
          crosses = t1 + t2 + h
       in if odd crosses then 1 else 0

main = do
  contents <- getContents
  let (start, grid) = parseGrid $ lines contents
      loop = walk grid 0 M.empty start
      part1 = (maximum (M.elems loop) + 1) `div` 2
      part2 = sum . map (inside grid loop) $ M.keys grid
  print part1
  print part2