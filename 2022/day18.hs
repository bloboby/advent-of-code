import qualified Data.Set as S
import Data.List.Split (splitOn)
import Data.Set (Set)

parse :: String -> [[Int]]
parse = map (map read . splitOn ",") . lines

getNbrs x = map (zipWith (+) x) [[1,0,0], [-1,0,0], [0,1,0], [0,-1,0], [0,0,1], [0,0,-1]]

countHidden interior = length . filter (`S.member` interior) . getNbrs

solve1 cubes =
  let cubeSet = S.fromList cubes
      hidden = sum $ map (countHidden cubeSet) cubes
  in 6 * S.size cubeSet - hidden

dfs _ _ _ _ [] = True
dfs cubes lo hi vis (x:xs)
  | minimum x < lo || maximum x > hi = False
  | x `S.member` cubes = False
  | x `S.member` vis = dfs cubes lo hi vis xs
  | otherwise = dfs cubes lo hi vis' $ nbrs ++ xs
  where vis' = S.insert x vis
        nbrs = filter (not . (`S.member` cubes)) $ getNbrs x

solve2 cubes =
  let cubeSet = S.fromList cubes
      lo = minimum $ concat cubes
      hi = maximum $ concat cubes
      space = [[a,b,c] | a <- [lo..hi], b <- [lo..hi], c <- [lo..hi]]
      -- inefficient DFS, doesn't memoise
      isInterior x = dfs cubeSet lo hi S.empty [x]
      interior = S.fromList $ filter isInterior space
      hidden = sum $ map (countHidden $ S.union cubeSet interior) cubes
  in 6 * S.size cubeSet - hidden

main = do
  contents <- getContents
  let cubes = parse contents
  print $ solve1 cubes
  print $ solve2 cubes
