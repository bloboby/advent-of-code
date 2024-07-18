import Data.Array (listArray, range, (!))
import Data.List (intercalate)
import Data.List.Split (splitOn)

kFolds :: Int = 5

preprocess :: String -> (String, [Int])
preprocess x =
  let [a, b] = words x
      row = '.' : (intercalate "?" . replicate kFolds $ a)
      parts = concat . replicate kFolds . map read $ splitOn "," b
   in (row, parts)

solve :: ([Char], [Int]) -> Int
solve (row, parts) = go m n
  where
    (m, n) = (length row, length parts)
    row' = listArray (0, m) row
    parts' = listArray (0, n) parts

    isDot x = x == '.' || x == '?'
    isHash x = x == '#' || x == '?'

    go i 0 = if all isDot (take i row) then 1 else 0
    go 0 j = 0
    go i j =
      let (r, p) = (row' ! (i - 1), parts' ! (j - 1))
          step = if isDot r then cache ! (i - 1, j) else 0
          x : xs = drop (i - p - 1) $ take i row
          match
            | length xs < p = 0
            | not (isDot x && all isHash xs) = 0
            | otherwise = cache ! (i - p - 1, j - 1)
       in step + match

    cache = listArray bounds [go i j | (i, j) <- range bounds]
    bounds = ((0, 0), (m, n))

main = do
  contents <- getContents
  let puzzles = map preprocess $ lines contents
      ans = sum $ map solve puzzles
  print ans
