import Data.Char (ord)
import Data.List (findIndex)
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Map qualified as M

data Step = Equals String Int | Dash String deriving (Show)

type Lens = (String, Int)

hash :: String -> Int
hash x =
  let f acc c = (acc + ord c) * 17 `mod` 256
   in foldl f 0 x

parse :: String -> Step
parse x
  | last x == '-' = Dash . init $ x
  | otherwise =
      let [a, b] = splitOn "=" x
       in Equals a (read b)

op :: Map Int [Lens] -> Step -> Map Int [Lens]
op boxes (Dash x) = M.adjust (filter ((/= x) . fst)) (hash x) boxes
op boxes (Equals x n) =
  let k = hash x
      idx = M.lookup k boxes >>= findIndex ((== x) . fst)
   in case idx of
        Just i ->
          let box = boxes M.! k
              box' = take i box ++ [(x, n)] ++ drop (i + 1) box
           in M.insert k box' boxes
        _ -> M.insertWith (flip (++)) k [(x, n)] boxes

power :: (Int, [Lens]) -> Int
power (n, xs) = sum . zipWith (\a b -> a * b * (n + 1)) [1 ..] $ map snd xs

main = do
  contents <- getContents
  let steps = splitOn "," . head $ lines contents
      part1 = sum $ map hash steps
      boxes = foldl op M.empty $ map parse steps
      part2 = sum . map power $ M.toList boxes
  print part1
  print part2
