import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map (Map)
import Data.Set (Set)

import Data.Char (ord)
import Data.List (transpose)

type State = (Map Int Char, Map Char String)
type Path = [(Int, State)]

-- input is hardcoded

makeState :: [String] -> State
makeState rooms = (M.fromList . zip [0..10] $ repeat '.',
                   M.fromList . zip "ABCD" $ rooms)

kRoomSize = 4
input s = makeState ["ADDD", "CCBD", "BBAB", "AACC"]

-- compute graph

getNbrs :: State -> [(Int, State)]
getNbrs s@(hall, _) = a ++ b
  where a = concatMap (moveHall s) $ M.toList hall
        b = concatMap (moveRoom s) $ "ABCD"

getBounds :: Map Int Char -> Int -> [Int]
getBounds hall i = map g [lf $ reverse h, rf h]
  where
    h = M.toList $ M.insert i '.' hall
    g = fst . last . takeWhile ((=='.') . snd)
    lf = dropWhile ((>i).fst)
    rf = dropWhile ((<i).fst)

moveHall :: State -> (Int, Char) -> [(Int, State)]
moveHall (hall, rooms) (i, c)
  | c == '.' = []
  | all (==c) room && left <= 2*n && 2*n <= right =
      let dist = 10^(n-1) * (abs (i-2*n) + kRoomSize - length room)
          hall' = M.insert i '.' hall
          rooms' = M.insert c (c:room) rooms
      in [(dist, (hall', rooms'))]
  | otherwise = []
  where room = rooms M.! c
        n = ord c - ord 'A' + 1
        [left, right] = getBounds hall i

moveRoom :: State -> Char -> [(Int, State)]
moveRoom (hall, rooms) k
  | all (==k) room = []
  | left <= n && n <= right =
      let c = head room
          goto j =
            let dist = let energy = 10^(ord c - ord 'A')
                           const = 1 + kRoomSize - length room
                       in energy * (abs (j-n) + const)
                hall' = M.insert j c hall
                rooms' = M.insert k (tail room) rooms
            in (dist, (hall', rooms'))
      in map goto [x | x <- [left..right], odd x || x==0 || x==10]
  | otherwise = []
  where room = rooms M.! k
        n = 2 * (ord k - ord 'A' + 1)
        [left, right] = getBounds hall n

-- dijkstra with Set as PQ

dijkstra :: Map State Path -> Set State -> Set (Int, State) -> Path
dijkstra cache visited pq
  | current == (makeState $ map (take kRoomSize . repeat) "ABCD") =
      cache M.! current
  | current `S.member` visited = dijkstra cache visited pq'
  | otherwise =
      let weightNbrs = let notVis n = (snd n) `S.notMember` visited'
                       in filter notVis $ getNbrs current
          nbrs = map snd weightNbrs
          paths = map getPath weightNbrs
          cache' = foldr (uncurry M.insert) cache $ zip nbrs paths
          visited' = S.insert current visited
          pq'' = foldr S.insert pq' $ zip (map (fst.head) paths) nbrs
      in dijkstra cache' visited' pq''
  where
    ((d, current), pq') = S.deleteFindMin pq
    path = cache M.! current
    getPath (w,n) =
      case M.lookup n cache of
        Nothing -> (w+d,n):path
        Just path' -> let d' = fst $ head path'
                      in if w+d < d' then (w+d,n):path else path'

solve x = dijkstra (M.singleton x []) S.empty (S.singleton (0,x))

-- display

displayStep :: (Int, State) -> [String]
displayStep (dist, (hall, rooms)) =
  [show dist, "#############", "#" ++ (M.elems hall) ++ "#",
   f $ head rows] ++ (map g $ tail rows) ++ ["  #########", ""]
  where rows = transpose . map pad $ M.elems rooms
        pad room = let rest = kRoomSize - length room
                   in (take rest $ repeat '.') ++ room
        f [a,b,c,d] = "###" ++ (a:'#':b:'#':c:'#':d:"###")
        g [a,b,c,d] = "  #" ++ (a:'#':b:'#':c:'#':d:"#")

display = concatMap displayStep . reverse

main = interact $ unlines . display . solve . input
