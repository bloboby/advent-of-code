extrapolate :: [Int] -> Int
extrapolate xs
  | all (== 0) xs = 0
  | otherwise =
      let diffs = zipWith (-) (tail xs) xs
       in last xs + extrapolate diffs

main = do
  contents <- getContents
  let hs :: [[Int]] = map (map read . words) $ lines contents
      part1 = sum $ map extrapolate hs
      part2 = sum $ map (extrapolate . reverse) hs
  print part1
  print part2