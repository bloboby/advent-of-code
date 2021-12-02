import Data.List
import qualified Data.IntSet as S

getRow :: [Char] -> Int -> Int -> String -> Int
getRow _ lo _ [] = lo
getRow [a,b] lo hi (x:xs)
    | x == a = getRow [a,b] lo mid xs
    | x == b = getRow [a,b] mid hi xs
  where
    mid = (lo + hi) `div` 2

getSeat :: String -> Int
getSeat s = 8 * row + col
  where
    row = getRow "FB" 0 128 (take 7 s)
    col = getRow "LR" 0 8 (drop 7 s)

findEmpty :: [Int] -> [Int]
findEmpty taken = map succ $ filter p taken
  where
    p m = (m+1) `S.notMember` t && (m+2) `S.member` t
    t = S.fromList taken

main = interact $ show.findEmpty.map getSeat.lines

