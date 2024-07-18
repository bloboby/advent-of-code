import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map (Map)
import Data.Set (Set)

import Data.List (transpose)
import Data.List.Split (splitOn)
import Data.Maybe (mapMaybe)
import Util

type Edges = Set String
type Jigsaw = Map Int ([Int], [String]) -- id: (coords, grid)
type Grid = Map [Int] Char

-- preprocess matches

parseTile :: String -> (Int, [String])
parseTile s = (id, grid)
  where (header:grid) = lines s
        id = read . init . last $ words header

getEdges :: [String] -> Edges
getEdges g = S.fromList $ e ++ map reverse e
  where e = [head g, last g, map head g, map last g]

getMatches :: Map Int Edges -> Int -> Edges -> [Int]
getMatches allEdges n edges = filter (/=n) $ M.keys matches
  where matches = M.filter (not . S.disjoint edges) allEdges

preprocess :: String -> (Map Int [String], Map Int [Int])
preprocess s = (grids, matches)
  where grids = M.fromList . map parseTile $ splitOn("\n\n") s
        matches = let edges = M.map getEdges grids
                  in M.mapWithKey (getMatches edges) edges

-- solve the jigsaw

reorient :: [String] -> [[String]]
reorient grid = rotations ++ map reverse rotations
  where r90 = map reverse . transpose
        rotations = take 4 $ iterate r90 grid

translate :: [String] -> [String] -> Maybe ([Int], [String])
translate old new
  | last old == head new = Just ([1,0], new)
  | head old == last new = Just ([-1,0], new)
  | map last old == map head new = Just ([0,1], new)
  | map head old == map last new = Just ([0,-1], new)
  | otherwise = Nothing

buildJigsaw ::
  Map Int [String]  {- readonly grids       -} ->
  Map Int [Int]     {- readonly matches     -} ->
  [Int]             {- unprocessed id queue -} ->
  Jigsaw            {- accumulator          -} ->
  Jigsaw
buildJigsaw _ _ [] acc = acc
buildJigsaw grids matches (n:queue) acc =
  buildJigsaw grids matches queue' acc'
  where ([x,y], grid) = acc M.! n
        nbrs = filter (`M.notMember` acc) $ matches M.! n
        f nbr = let gs = reorient $ grids M.! nbr
                    ([dx,dy], g) = head $ mapMaybe (translate grid) gs
                in (nbr, ([x+dx, y+dy], g))
        queue' = queue ++ nbrs
        acc' = foldr (uncurry M.insert) acc $ map f nbrs

-- postprocess

rekey :: [Int] -> [Int] -> [Int] -> [String] -> [([Int], Char)]
rekey size mins coords grid = kvs
  where [x,y] = zipWith (*) size $ zipWith (-) coords mins
        kvs = [([a,b],v) | (a,row) <- zip [x..] grid,
                           (b,v) <- zip [y..] row]

postprocess :: Jigsaw -> Grid
postprocess jigsaw = M.fromList image
  where strip grid = let f = init . tail in map f $ f grid
        j = M.map strip . M.fromList $ M.elems jigsaw
        size = let g = j M.! [0,0] in [length g, length $ head g]
        mins = fst $ M.findMin j
        image = concat . M.elems $ M.mapWithKey (rekey size mins) j

assemble :: (Map Int [String], Map Int [Int]) -> Grid
assemble (grids, matches) = postprocess jigsaw
  where x = head $ M.keys grids
        acc = M.singleton x ([0,0], grids M.! x)
        jigsaw = buildJigsaw grids matches [x] acc

-- find monsters

stringify :: Grid -> [String]
stringify grid = s
  where [xmax, ymax] = fst $ M.findMax grid
        s = [[grid M.! [x,y] | x <- [0..xmax]]
                              | y <- [0..ymax]]

gridify :: [Int] -> [String] -> Grid
gridify [x,y] s = M.fromList kvs
  where kvs = [([a,b],v) | (a,row) <- zip [x..] s,
                           (b,v) <- zip [y..] row]

monsterAt :: [Int] -> Grid
monsterAt [x,y] = M.filter (=='#') $ gridify [x,y] m
  where m = ["                  # ",
             "#    ##    ##    ###",
             " #  #  #  #  #  #   "]

findMonsters :: Grid -> Maybe Grid
findMonsters grid = if null monsters then Nothing
                    else Just $ M.unions monsters
  where hashes = M.filter (=='#') grid
        candidates = map monsterAt $ M.keys grid
        monsters = filter (flip M.isSubmapOf hashes) candidates

solve :: Grid -> Int
solve image = length hashes - length monsters
  where images = map (gridify [0,0]) . reorient . stringify $ image
        monsters = head $ mapMaybe findMonsters images
        hashes = M.filter (=='#') image

main = interact $ Util.print . solve . assemble . preprocess
