import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char (isDigit)
import Data.Map (Map)
import Data.Maybe (catMaybes)
import Data.Set (Set)

type Flows = Map String Int
type Adj = Map String [String]

-- (pressure, time, pos1, pos2, opened valves)
type State = (Int, Int, String, String, Set String)
type Node = (Int, State)

endTime = 26

parseLine l =
  let (_:id:_:_:rate:_:_:_:_:nbrs) = words l
      rate' = read $ filter isDigit rate
      nbrs' = map (filter (/=',')) nbrs
  in ((id, rate'), (id, nbrs'))

parse :: String -> (Flows, Adj)
parse contents =
  let ls = map parseLine $ lines contents
      flows = M.fromList . filter ((/=0) . snd) $ map fst ls
      adj = M.fromList $ map snd ls
  in (flows, adj)

-- A* with Set as max PQ
search flows adj vis best pq
  | h <= best = best
  | (x1, x2, open) `S.member` vis = search flows adj vis best pq'
  | t == endTime || S.size open == M.size flows =
      search flows adj vis' (max p best) pq'
  | otherwise = search flows adj vis' best pq''
  where
    ((h, state@(p, t, x1, x2, open)), pq') = S.deleteFindMax pq
    vis' = S.insert (x2, x1, open) $ S.insert (x1, x2, open) vis

    nbrStates = [combine n1 n2 | n1 <- getNbrs x1, n2 <- getNbrs x2]
    nbrs = map (\x -> (heuristic x, x)) nbrStates
    pq'' = foldr S.insert pq' nbrs

    -- Helpers
    getNbrs :: String -> [(String, Maybe String)]
    getNbrs x =
      let valid = (not $ x `S.member` open) && (x `M.member` flows)
          moves = zip (adj M.! x) (repeat Nothing)
      in if valid then (x, Just x):moves else moves

    getP x = case x of
      Just a -> (flows M.! a) * (endTime - t)
      Nothing -> 0

    combine (x1', o1) (x2', o2) =
      let [p1, p2] = map getP [o1, o2]
          p' = if o1 /= Nothing && o1 == o2
               then p + p1
               else p + p1 + p2
          open' = foldr S.insert open $ catMaybes [o1, o2]
      in (p', t+1, x1', x2', open')

    -- Heuristic: immediately open the rest.
    totalFlow o = sum . M.elems $ M.filterWithKey (\k _ -> not $ k `S.member` o) flows
    heuristic (p', t', _, _, o) = p' + (totalFlow o) * (endTime - t')

solve (flows, adj) =
  let start = S.singleton (1, (0, 1, "AA", "AA", S.empty))
  in search flows adj S.empty 0 start

main = interact $ show . solve . parse
