import Data.Char (digitToInt, isDigit, ord)

type Plan = (Char, Int)

hexToDec :: String -> Int
hexToDec = foldl (\acc c -> acc * 16 + f c) 0
  where
    f c = if isDigit c then ord c - ord '0' else ord c - ord 'a' + 10

parse1 :: String -> Plan
parse1 line =
  let [[a], b, _] = words line
   in (a, read b)

parse2 :: String -> Plan
parse2 line =
  let [_, _, _ : _ : x] = words line
      c = "RDLU" !! digitToInt (x !! 5)
      n = hexToDec $ take 5 x
   in (c, n)

dblArea :: [Plan] -> Int
dblArea = abs . fst . foldl f (0, (0, 0))
  where
    f (acc, (x, y)) (c, n) =
      let (x', y') = move c n (x, y)
       in (acc + (y + y') * (x - x'), (x', y'))

    move c n (x, y) = case c of
      'U' -> (x - n, y)
      'D' -> (x + n, y)
      'L' -> (x, y - n)
      'R' -> (x, y + n)

solve :: [Plan] -> Int
solve plans =
  let a = dblArea plans
      b = sum $ map snd plans
      i = (a - b + 2) `div` 2
   in b + i

main :: IO ()
main = do
  contents <- getContents
  let plans1 = map parse1 $ lines contents
      plans2 = map parse2 $ lines contents
  print $ solve plans1
  print $ solve plans2