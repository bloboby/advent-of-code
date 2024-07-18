import Data.Char (digitToInt)
import Data.List (transpose)
import Util

binToDec :: [Int] -> Int
binToDec bin = addDigit (reverse bin) 1 0
  where addDigit [] _ dec = dec
        addDigit (x:xs) i dec = addDigit xs (i*2) (dec+x*i)

getRating :: Bool -> Int -> [String] -> Int
getRating _ _ [row] = binToDec $ map digitToInt row
getRating useMost i rows = getRating useMost (i+1) (filter cond rows)
  where cond row = (row!!i == most!!i) == useMost
        most = map getMost $ transpose rows
        getMost col = let count x = length $ filter (==x) col
          in if count '1' >= count '0' then '1' else '0'

getRatings :: [String] -> [Int]
getRatings r = [getRating True 0 r, getRating False 0 r]

main = interact $ Util.print . product . getRatings . lines
