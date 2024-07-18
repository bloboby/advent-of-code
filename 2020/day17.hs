import Data.List
import Data.Maybe
import qualified Data.Map as M

type Index = (Int,Int,Int,Int)
type State = M.Map Index Char


parse :: [String] -> State
parse = foldl parseRow M.empty.zip [0..]
  where
    parseRow acc (i, row) = foldl (parseCell i) acc $ zip [0..] row
    parseCell i acc (j, cell) = M.insert (0,0,i,j) cell acc


getNbrs :: Index -> [Index]
getNbrs (w,x,y,z) = delete (w,x,y,z) nbrs
  where
    nbrs = [(i,j,k,l) | i<-f w, j<-f x, k<-f y, l<-f z]
    f n = [n-1..n+1]

addInactive :: Index -> State -> State
addInactive idx acc
    | idx `M.member` acc = acc
    | otherwise = M.insert idx '.' acc

updateState :: State -> Index -> State -> State
updateState state idx acc
    | (state M.! idx) == '#' && n == 2 = M.insert idx '#' acc
    | otherwise = M.insert idx (if n==3 then '#' else '.') acc
  where
    n = length.filter (=='#').mapMaybe (flip M.lookup state) $ getNbrs idx

countActive :: Int -> State -> Int
countActive numIters =
    length.filter (=='#').M.elems.(!!numIters).iterate doCycle
  where
    doCycle state =
        let nbrs = concat.map getNbrs $ M.keys state
            state' = foldr addInactive state nbrs
        in foldr (updateState state') M.empty $ M.keys state'


main = interact $ show.countActive 6.parse.filter (not.null).lines

