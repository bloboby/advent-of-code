import qualified Data.Map as M
import Data.Char (digitToInt, isAlphaNum)
import Data.List (transpose)
import Data.List.Split (splitOn)
import Data.Map (Map)

parseStacks :: String -> Map Int [Char]
parseStacks = M.fromList . map (\s -> (digitToInt $ last s, init s))
  . filter (not . null) . map (filter isAlphaNum) . transpose . lines

parseMoves :: String -> [[Int]]
parseMoves = map ((\[_,a,_,b,_,c] -> map read [a,b,c]) . words) . lines

doMove :: Map Int [Char] -> [Int] -> Map Int [Char]
doMove m [n,a,b] =
  let (from, to) = (m M.! a, m M.! b)
      (moved, from') = (take n from, drop n from)
      to' = moved ++ to  -- reverse moved ++ to
  in M.insert a from' . M.insert b to' $ m

main = do
  contents <- getContents
  let [rawStacks, rawMoves] = splitOn "\n\n" contents
  let stacks = parseStacks rawStacks
  let moves = parseMoves rawMoves
  putStrLn . map head . M.elems $ foldl doMove stacks moves
