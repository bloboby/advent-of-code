import Data.Bifunctor (bimap)
import Data.List (transpose)
import Data.Map (Map)
import Data.Map qualified as M

type Coord = (Int, Int)

kFactor :: Int = 1000000

parseGrid :: [[Char]] -> Map Coord Char
parseGrid rows =
  let f = zip [0 ..]
   in M.fromList [((i, j), x) | (i, row) <- f rows, (j, x) <- f row]

galaxies :: [[Char]] -> [Coord]
galaxies rows =
  let galaxies = M.keys . M.filter (== '#') $ parseGrid rows
      f empties n = n + (kFactor - 1) * length (takeWhile (< n) empties)
      empty = map fst . filter (all (== '.') . snd) . zip [0 ..]
      [er, ec] = map empty [rows, transpose rows]
   in map (bimap (f er) (f ec)) galaxies

dists :: [Coord] -> Int
dists xs =
  let d (a, b) (c, d) = abs (a - c) + abs (b - d)
   in sum [d x y | x <- xs, y <- xs, x < y]

main = do
  contents <- getContents
  let rows = lines contents
      ans = dists $ galaxies rows
  print ans