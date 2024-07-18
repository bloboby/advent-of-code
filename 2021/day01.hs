import Util

solve :: [Int] -> Int
solve ns = length $ filter (\(x,y) -> x<y) $ zip ns (drop 3 ns)

main = interact $ Util.print . solve . map read . words
