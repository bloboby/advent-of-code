import Data.Char (isLower, isUpper, ord)
import Data.Set (Set)
import qualified Data.Set as S
import Util

setToChar = head . S.elems

score x
  | isLower x = ord x - ord 'a' + 1
  | isUpper x = ord x - ord 'A' + 27

-- Part 1

getCommon :: String -> Char
getCommon s =
  let (a,b) = splitAt (div (length s) 2) s
  in setToChar $ S.intersection (S.fromList a) (S.fromList b)

-- Part 2

getBadges :: [String] -> [Char]
getBadges [] = []
getBadges (x:y:z:xs) =
  let badge = setToChar $ foldl1 S.intersection (map S.fromList [x,y,z])
  in badge : getBadges xs

main = interact $ Util.print . sum . map score . getBadges . lines
