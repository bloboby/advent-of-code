import qualified Data.Map as M
import Data.Map (Map)

data Job = Lone Int | Wait [String] deriving (Show)

parseLine :: String -> (String, Job)
parseLine s
  | length xs == 1 = (init x, Lone (read $ head xs))
  | otherwise = (init x, Wait xs)
  where (x:xs) = words s

execute :: Map String Job -> String -> Int
execute jobs monkey = case jobs M.! monkey of
  Lone n -> n
  Wait [x, op, y] ->
    let [a, b] = map (execute jobs) [x, y]
    in case op of
         "+" -> a + b
         "-" -> a - b
         "*" -> a * b
         "/" -> a `div` b

test jobs n =
  let jobs' = M.insert "humn" (Lone n) jobs
      Wait [a, _, b] = jobs' M.! "root"
  in execute jobs' b - execute jobs' a

search jobs lo hi
  | n == 0 = mid
  | n > 0 = search jobs lo mid
  | n < 0 = search jobs mid hi
  where mid = (lo + hi) `div` 2
        n = test jobs mid

main = do
  contents <- getContents
  let jobs = M.fromList . map parseLine $ lines contents
  print $ execute jobs "root"
  print $ search jobs 0 10000000000000
