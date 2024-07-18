import Data.Array (Array, listArray, range, (!))
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as S

type Coord = (Int, Int)

type TCoord = (Int, Int, Int) -- (x, y, t)

type Blizzards = Array TCoord Bool

blizzards :: [String] -> Blizzards
blizzards rows = cache'
  where
    n = length rows - 2
    m = length (head rows) - 2
    end = lcm n m

    cache = listArray bounds [go t | t <- range bounds]
    bounds = (0, end)
    go t = S.fromList $ map ($ t) pos

    pos =
      let op (x, y) '^' t = (wrap n (x - t), y)
          op (x, y) 'v' t = (wrap n (x + t), y)
          op (x, y) '<' t = (x, wrap m (y - t))
          op (x, y) '>' t = (x, wrap m (y + t))

          z = zip [0 ..]
          wrap n x = (((x - 1) `mod` n) + n) `mod` n + 1
       in [op (i, j) c | (i, row) <- z rows, (j, c) <- z row, c `elem` "^v<>"]

    cache' = listArray bounds' [go' x y t | (x, y, t) <- range bounds']
    bounds' = ((0, 0, 0), (n, m, end))
    go' x y t = not $ (x, y) `S.member` (cache ! t)

solve :: Blizzards -> [String] -> Coord -> Coord -> Int -> Int
solve bs rows start@(sx, sy) end time = bfs S.empty [(sx, sy, time)]
  where
    n = length rows
    m = length $ head rows
    tmod = lcm (n - 2) (m - 2)

    bfs :: Set TCoord -> [TCoord] -> Int
    bfs vis ((x, y, t) : xs)
      | (x, y) == end = t
      | (x, y, t) `S.member` vis = bfs vis xs
      | otherwise = bfs vis' (xs ++ nbrs)
      where
        vis' = S.insert (x, y, t) vis
        adj = [(x, y), (x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
        nbrs = filter clear $ map (\(i, j) -> (i, j, t + 1)) adj

    clear :: TCoord -> Bool
    clear (x, y, t)
      | (x, y) == start || (x, y) == end = True
      | x < 1 || y < 1 || x >= n - 1 || y >= m - 1 = False
      | otherwise = bs ! (x, y, t `mod` tmod)

main :: IO ()
main = do
  contents <- getContents
  let rows = lines contents
      bs = blizzards rows

      start :: Coord = (0, fromJust $ elemIndex '.' (head rows))
      end :: Coord = (length rows - 1, fromJust $ elemIndex '.' (last rows))

      leg1 = solve bs rows start end 0
      leg2 = solve bs rows end start leg1
      leg3 = solve bs rows start end leg2

  print leg1
  print leg3
