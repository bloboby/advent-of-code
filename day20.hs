import Data.List
import Data.List.Split
import qualified Data.Map as M
import qualified Data.Set as S

type Edges = S.Set String
type Tile = (Int, Edges)


parse :: String -> Tile
parse s = (id, edges)
  where
    id = read.init.last.words.head $ lines s
    g = tail $ lines s
    e = [head g, last g, map head g, map last g]
    edges = S.fromList $ e ++ map reverse e

--countMatches :: [Tile] -> Tile -> Int
countMatches tiles (id, edges) = length (foldr match [] tiles) - 1
  where
    match (id', edges') acc =
        if S.null $ S.intersection edges edges' then acc
        else (id':acc)

--findCorners :: [Tile] -> Int
findCorners tiles = product.map fst $ filter ((==2).snd) degs
  where
    degs = map (\t -> (fst t, countMatches tiles t)) tiles



-- primitive matching gives 4/40/100 for degrees 2/3/4,
-- hence all matches are final

main = interact $ show
    .findCorners
    .map parse
    .filter (not.null).splitOn("\n\n")

