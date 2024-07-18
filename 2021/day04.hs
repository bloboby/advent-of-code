import qualified Data.Set as S
import Data.List (transpose)
import Data.List.Split (splitOn)
import Util

type Board = (S.Set (S.Set Int), S.Set Int)

makeBoard :: [[Int]] -> Board
makeBoard grid = (makeRows grid, makeUnmarked grid)
  where makeRows grid = let makeSet = S.fromList . map S.fromList
          in S.unions $ map makeSet [grid, transpose grid]
        makeUnmarked = S.unions . map S.fromList

preprocess :: String -> ([Int], [Board])
preprocess input = (numbers, map makeBoard grids)
  where paragraphs = splitOn "\n\n" input
        numbers = map read . splitOn "," $ head paragraphs
        grids = map (map (map read.words).lines) $ tail paragraphs

drawNumber :: Int -> Board -> Board
drawNumber n (rows, unmarked) = (rows', unmarked')
  where rows' = S.map (S.delete n) rows
        unmarked' = S.delete n unmarked

playBingo :: ([Int], [Board]) -> Int
playBingo ((n:ns), boards) =
  if null boards' then n * (sum . S.toList . snd $ head updated)
                  else playBingo (ns, boards')
  where updated = map (drawNumber n) boards
        boards' = filter (S.notMember S.empty . fst) $ updated

main = interact $ Util.print . playBingo . preprocess
