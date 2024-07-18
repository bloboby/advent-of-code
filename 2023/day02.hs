import Data.List.Split (splitOn, splitOneOf)

type Cubes = [Int] -- [red, green, blue]

type Game = [Cubes]

parseCubes :: String -> Cubes
parseCubes x =
  let cubes = map words $ splitOn "," x
      f [n, col] [r, g, b] =
        case col of
          "red" -> [read n, g, b]
          "green" -> [r, read n, b]
          "blue" -> [r, g, read n]
   in foldr f [0, 0, 0] cubes

parseGame :: String -> Game
parseGame = map parseCubes . tail . splitOneOf ":;"

part1 :: [Game] -> Int
part1 games =
  let isPossible = and . zipWith (>=) [12, 13, 14]
      possible = map (all isPossible) games
      ids = map fst . filter snd $ zip [1 ..] possible
   in sum ids

part2 :: [Game] -> Int
part2 games =
  let minCubes = foldr (zipWith max) [0, 0, 0]
      powers = map (product . minCubes) games
   in sum powers

main = do
  contents <- getContents
  let games = map parseGame $ lines contents
  print (part1 games)
  print (part2 games)