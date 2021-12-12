import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Util

type Cube = (Bool, [[Int]])
-- (sign, [[x1,x2], [y1,y2], [z1,z2]])

parse :: String -> Cube
parse s = (cmd == "on", f coords)
  where [cmd, coords] = words s
        f = map (map read . splitOn ".." . drop 2) . splitOn ","

intersect :: Cube -> Cube -> Maybe Cube
intersect (_, new) (sign, old) =
  if disjoint then Nothing else Just intersection
  where f [a,b] [c,d] = b<c || d<a
        g [a,b] [c,d] = [max a c, min b d]
        disjoint = or $ zipWith f new old
        intersection = (not sign, zipWith g new old)

execute :: [Cube] -> Cube -> [Cube]
execute grid cube = if fst cube then cube:grid' else grid'
  where grid' = grid ++ mapMaybe (intersect cube) grid

count :: Cube -> Int
count (sign, cube) = if sign then vol else -vol
  where vol = product $ map (\[a,b] -> b-a+1) cube

solve = sum . map count . foldl execute [] . map parse . lines

main = interact $ Util.print . solve
