parseInput :: String -> [(Int, Int)]
parseInput x =
  let [t, d] = map (map read . tail . words) . lines $ x
   in zip t d

parseInput' :: String -> (Int, Int)
parseInput' x =
  let [t, d] = map (read . concat . tail . words) . lines $ x
   in (t, d)

waysToWin :: (Int, Int) -> Int
waysToWin (t, d)
  | disc < 0 = 0
  | otherwise = t - 2 * minSol + 1
  where
    disc = t * t - 4 * d
    minSol = floor $ (fromIntegral t - sqrt (fromIntegral disc)) / 2 + 1

main = do
  contents <- getContents
  let input = parseInput contents
      input' = parseInput' contents
      part1 = product $ map waysToWin input
      part2 = waysToWin input'
  print part1
  print part2