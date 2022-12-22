import qualified Data.Map as M
import Data.List (foldl')
import Data.List.Split (splitOn)
import Data.Map (Map)
import Text.ParserCombinators.Parsec

type Grid = Map [Int] Char
data Cmd = L | R | Fwd Int deriving (Show)

parseCmds :: String -> [Cmd]
parseCmds s =
  let cmd = l <|> r <|> fwd
      l = char 'L' >> return L
      r = char 'R' >> return R
      fwd = Fwd . read <$> many1 digit
  in case parse (many1 cmd) "" s of
       Right cmds -> cmds
       Left err -> undefined

parseContents :: String -> (Grid, [Cmd])
parseContents s =
  let [a,b] = splitOn "\n\n" s
      grid = M.fromList . filter ((/=' ') . snd)
               $ [([i, j], x) | (i, row) <- zip [1..] $ lines a,
                                (j, x) <- zip [1..] row]
  in (grid, parseCmds b)

delta facing = case facing of
  0 -> [0,1]
  1 -> [1,0]
  2 -> [0,-1]
  3 -> [-1,0]

wrap1 grid [x,y] facing = (xy, facing)
  where xs = M.keys $ M.filterWithKey (\[a,_] _ -> a == x) grid
        ys = M.keys $ M.filterWithKey (\[_,b] _ -> b == y) grid
        xy = case facing of
          0 -> minimum xs
          1 -> minimum ys
          2 -> maximum xs
          3 -> maximum ys

wrap2 grid [x,y] facing
  | x == 0 && y <= 100      = ([y+100, 1], 0)   -- 1 -> 10
  | x == 0                  = ([200, y-100], 3) -- 2 -> 9
  | y == 151                = ([151-x, 100], 2) -- 3 -> 6
  | x == 51 && facing == 1  = ([y-50, 100], 2)  -- 4 -> 5
  | y == 101 && x <= 100    = ([50, x+50], 3)   -- 5 -> 4
  | y == 101                = ([151-x, 150], 2) -- 6 -> 3
  | x == 151 && facing == 1 = ([y+100, 50], 2)  -- 7 -> 8
  | y == 51                 = ([150, x-100], 3) -- 8 -> 7
  | x == 201                = ([1, y+100], 1)   -- 9 -> 2
  | y == 0 && x >= 151      = ([1, x-100], 1)   -- 10 -> 1
  | y == 0                  = ([151-x, 51], 0)  -- 11 -> 14
  | x == 100 && facing == 3 = ([y+50, 51], 0)   -- 12 -> 13
  | y == 50 && x >= 51      = ([101, x-50], 1)  -- 13 -> 12
  | y == 50                 = ([151-x, 1], 0)   -- 14 -> 11

move grid (xy, facing) L = (xy, (facing - 1) `mod` 4)
move grid (xy, facing) R = (xy, (facing + 1) `mod` 4)
move grid (xy, facing) (Fwd n)
  | n == 0 = (xy, facing)
  | grid M.! xy' == '#' = (xy, facing)
  | otherwise = move grid (xy', facing') $ Fwd (n-1)
  where temp = zipWith (+) xy $ delta facing
        (xy', facing') = if temp `M.member` grid then (temp, facing)
                         else wrap2 grid temp facing

solve (grid, cmds) =
  let start = fst $ M.findMin grid
      ([x,y], facing) = foldl' (move grid) (start, 0) cmds
  in 1000 * x + 4 * y + facing

main = interact $ show . solve . parseContents
