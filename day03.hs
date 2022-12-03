import Data.Char (isLower, isUpper, ord)
import Data.Set (Set)
import qualified Data.Set as S
import Util

setToChar = head . S.elems

score x
  | isLower x = ord x - ord 'a' + 1
  | isUpper x = ord x - ord 'A' + 27

getCommon s =
  let (a,b) = splitAt (div (length s) 2) s
  in setToChar $ S.intersection (S.fromList a) (S.fromList b)

getBadges [] = []
getBadges (x:y:z:xs) =
  let badge = setToChar $ foldl1 S.intersection (map S.fromList [x,y,z])
  in badge : getBadges xs

solve1 = sum . map (score . getCommon)
solve2 = sum . map score . getBadges
main = interact $ Util.print . solve2 . lines
