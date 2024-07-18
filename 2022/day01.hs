import Data.List (sort)
import Data.List.Split (splitOn)
import Util

main = interact $ Util.print
  . sum . take 3 . reverse . sort
  . map (sum . map read . words) . splitOn "\n\n"
