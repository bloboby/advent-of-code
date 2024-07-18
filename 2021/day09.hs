import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char (digitToInt)
import Data.List (sort)
import Data.Maybe (fromMaybe, mapMaybe)
import Util

type Grid = M.Map [Int] Int
type Points = S.Set [Int]

preprocess :: String -> Grid
preprocess input = M.fromList kv
  where arr = map (map digitToInt) $ lines input
        kv = [([x,y],v) | (x,row) <- zip [0..] arr,
                          (y,v) <- zip [0..] row]

getNbrs :: [Int] -> [[Int]]
getNbrs pt = map (zipWith (+) pt) [[1,0],[-1,0],[0,1],[0,-1]]

isLow :: Grid -> [Int] -> Bool
isLow grid pt = and $ map (>v) vals
  where getVal k = M.lookup k grid
        v = fromMaybe 10 $ getVal pt
        vals = mapMaybe getVal $ getNbrs pt

getBasin :: Grid -> Points -> [Int] -> Points
getBasin grid basin pt =
  if S.member pt basin then basin
  else foldl (getBasin grid) (S.insert pt basin) nbrs
  where inBasin k = 9 > (M.findWithDefault 10 k grid)
        nbrs = filter inBasin $ getNbrs pt

solve :: Grid -> Int
solve grid = product . take 3 . reverse . sort $ sizes
  where lows = M.keys $ M.filterWithKey (\k _ -> isLow grid k) grid
        sizes = map (length . getBasin grid S.empty) lows

main = interact $ Util.print . solve . preprocess
