import qualified Data.Set as S
import Data.Char (isDigit, isSpace)
import Data.List (sort)

data Bound = L | R deriving (Eq, Ord, Show)

parseLine :: String -> [Int]
parseLine = map read . words . filter (\c -> isDigit c || isSpace c || c == '-')

getBounds y [a,b,c,d]
  | dx < 0 = []
  | otherwise = [(a-dx, L), (a+dx, R)]
  where dx = abs (a-c) + abs (b-d) - abs (b-y)

sweep :: [(Int, Bound)] -> Int -> Int -> [(Int, Int)] -> [(Int, Int)]
sweep [] _ 0 out = out
sweep ((r, R):xs) l 1 out =
  let out' = if (not $ null out) && ((snd $ head out) == l-1)
             then (fst $ head out, r):(tail out)
             else (l,r):out
  in sweep xs undefined 0 out'
sweep ((r, R):xs) l n out = sweep xs l (n-1) out
sweep ((l, L):xs) _ 0 out = sweep xs l 1 out
sweep ((_, L):xs) l n out = sweep xs l (n+1) out

getIntervals s y =
  let pts = sort . concat $ map (getBounds y) s
  in reverse $ sweep pts undefined 0 []

solve1 s =
  let y = 2000000
      count = sum . map (\(a,b) -> b-a+1) $ getIntervals s y
      exclude = length . S.fromList . map (!!2) $ filter ((==y) . last) s
  in count - exclude

solve2 s =
  let max = 4000000
      intervals = zip [0..] $ map (getIntervals s) [0..max]
      [(y, ((_,x):_))] = filter ((/=1) . length . snd) intervals
  in max * (x+1) + y

main = do
  contents <- getContents
  let sensors = map parseLine $ lines contents
      part1 = solve1 sensors
      part2 = solve2 sensors
  print part1
  print part2
