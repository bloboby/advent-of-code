import Data.Char (isDigit)
import Util

parse :: String -> [Int]
parse = map read . words . map remove
  where remove c = if c == '-' || isDigit c then c else ' '

solve [x1,x2,y1,y2] = length $ filter valid space
  where space = [(x,y) | x <- [0..x2], y <- [y1..(-y1)]]
        f [x,y,dx,dy] = [x+dx, y+dy, max 0 (dx-1), dy-1]
        sim (x,y) = takeWhile ((>=y1).(!!1)) . iterate f $ [0,0,x,y]
        valid = let inBox (x:y:_) = x>=x1 && x<=x2 && y>=y1 && y<=y2
                in not . null . filter inBox . sim

main = interact $ Util.print . solve . parse
