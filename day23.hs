import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map (Map)
import Data.Set (Set)

import Data.Char (ord)
import Data.Tuple (swap)
import Util

type State = (Map Int Char, Map Char String)

-- this code gives a wrong answer for the test input (too low, so
-- possibly generated an invalid move) but i cbb debugging any more

makeState :: [String] -> State
makeState rooms = (M.fromList . zip [0..10] $ repeat '.',
                   M.fromList . zip "ABCD" $ rooms)

input :: String -> State
-- input s = makeState ["BDDA", "CCBD", "BBAC", "DACA"]
input s = makeState ["ADDD", "CCBD", "BBAB", "AACC"]

kRoomSize = 4

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
  | otherwise =
      let slide j =
            let dist = 10^(n-1) * abs (j-i)
                hall' = M.insert j c . M.insert i '.' $ hall
            in (dist, (hall', rooms))
      in map slide [x | x <- [left..right], odd x || x==0 || x==10]
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

getDist :: Map State Int -> Int -> (Int, State) -> (Int, State)
getDist dist d (weight, node) =
  case M.lookup node dist of
    Nothing -> (new, node)
    Just old -> (min old new, node)
  where new = d + weight

dijkstra :: Map State Int -> Set State -> Set (Int, State) -> Int
dijkstra dist visited pq
  | current == (makeState $ map (take kRoomSize . repeat) "ABCD") = d
  | current `S.member` visited = dijkstra dist visited pq'
  | otherwise =
      let nbrs = let notVis n = (snd n) `S.notMember` visited'
                 in filter notVis $ getNbrs current
          nbrDists = map (getDist dist d) nbrs
          dist' = foldr (uncurry M.insert) dist $ map swap nbrDists
          visited' = S.insert current visited
          pq'' = foldr S.insert pq' nbrDists
      in dijkstra dist' visited' pq''
  where
    ((d, current), pq') = S.deleteFindMin pq

solve x = dijkstra M.empty S.empty $ S.singleton (0, x)

main = interact $ Util.print . solve . input
