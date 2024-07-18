import Data.Char (digitToInt, intToDigit)

parse x = case x of
  '=' -> (-2)
  '-' -> (-1)
  _ -> digitToInt x

unparse x = case x of
  3 -> '='
  4 -> '-'
  _ -> intToDigit x

snafuToDec :: String -> Int
snafuToDec snafu = dec
  where f digit (coeff, acc) = (5 * coeff, acc + parse digit * coeff)
        (_, dec) = foldr f (1,0) snafu

decToSnafu 0 = []
decToSnafu n = digit:(decToSnafu n')
  where digit = unparse $ n `mod` 5
        q = n `div` 5
        n' = if digit `elem` "-=" then q+1 else q

main = interact $ reverse . decToSnafu . sum . map snafuToDec . lines
