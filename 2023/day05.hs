import Data.List (sort)
import Data.List.Split (chunksOf, splitOn)
import Data.Set (fromList, toList)

type Map = [[Int]]

type Range = [Int] -- length 2

parseInput :: String -> ([Int], [Map])
parseInput contents =
  let (x : xs) = splitOn "\n\n" contents
      seeds = map read . tail . words $ x
      maps = map (map (map read . words) . tail . lines) xs
   in (seeds, maps)

convertOne :: Int -> Map -> Int
convertOne n [] = n
convertOne n ([a, b, l] : xs) =
  if b <= n && n < (b + l) then a + n - b else convertOne n xs

expandRanges :: Map -> Range -> [Range]
expandRanges ms [a, bIncl] =
  let b = bIncl + 1
      mid = concatMap (\[_, x, y] -> [x, x + y]) ms
      allPts = toList . fromList $ a : b : mid
      pts = takeWhile (<= b) . dropWhile (< a) . sort $ allPts
   in zipWith (\x y -> [x, y - 1]) pts (tail pts)

applyOne :: [Range] -> Map -> [Range]
applyOne rs ms =
  let rs' = concatMap (expandRanges ms) rs
   in chunksOf 2 . map (`convertOne` ms) $ concat rs'

main = do
  contents <- getContents
  let (seeds, maps) = parseInput contents
      seeds' = map (\[a, b] -> [a, a + b - 1]) $ chunksOf 2 seeds
      convert ms n = foldl convertOne n ms
      part1 = minimum $ map (convert maps) seeds
      part2 = minimum . concat $ foldl applyOne seeds' maps
  print part1
  print part2
