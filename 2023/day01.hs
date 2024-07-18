import Data.Char (isDigit)
import Data.List (isPrefixOf)
import Data.Maybe (catMaybes, fromJust, isJust, listToMaybe)

leftDigit :: [String] -> String -> Int
leftDigit nums x
  | isDigit $ head x = read [head x]
  | isJust strDigit = fromJust strDigit -- Remove this line for part 1
  | otherwise = leftDigit nums $ tail x
  where
    f num n = if num `isPrefixOf` x then Just n else Nothing
    strDigit = listToMaybe . catMaybes $ zipWith f nums [1 ..]

calVal :: String -> Int
calVal x =
  let nums = ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]
      l = leftDigit nums x
      r = leftDigit (map reverse nums) (reverse x)
   in 10 * l + r

main = do
  contents <- getContents
  let ans = sum . map calVal $ lines contents
  print ans