import Data.List (intercalate, transpose)
import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Map qualified as M

type Seen = Map [String] Int

roll :: String -> String -- towards 0
roll x =
  let chunks = splitOn "#" x
      f c = filter (== 'O') c ++ filter (== '.') c
   in intercalate "#" $ map f chunks

spin :: [String] -> [String]
spin rows =
  let n = map roll . transpose $ rows
      w = map roll . transpose $ n
      s = map (roll . reverse) . transpose $ w
      e = map (roll . reverse) . transpose $ s
   in reverse . map reverse $ e

run :: Seen -> [String] -> Int -> (Int, Int, Seen)
run seen rows n
  | rows `M.member` seen = (seen M.! rows, n, seen)
  | otherwise =
      let seen' = M.insert rows n seen
       in run seen' (spin rows) (n + 1)

rowsAt :: Int -> [String] -> [String]
rowsAt n rows =
  let (r, m, seen) = run M.empty rows 0
      n' = r + (n - r) `mod` (m - r)
   in head . M.keys $ M.filter (== n') seen

main = do
  contents <- getContents
  let rows = lines contents
      load = sum . map fst . filter ((== 'O') . snd) . zip [1 ..] . reverse
      part1 = sum . map (load . roll) . transpose $ rows
      part2 = sum . map load . transpose $ rowsAt 1000000000 rows
  print part1
  print part2