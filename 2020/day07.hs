import Data.List.Split
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

type Node = (String, String)

{- day 1 only

parse :: String -> (Node, [Node])
parse s = (container, contents)
  where
    x:xs = chunksOf 4 $ words s
    container = let [a,b,_,_] = x in (a,b)
    contents = if length (xs!!0) < 4 then []
        else map (\[_,a,b,_]->(a,b)) xs

updateNbrs :: Node -> Maybe [Node] -> Maybe [Node]
updateNbrs nbr maybe_nbrs = case maybe_nbrs of
    Nothing -> Just [nbr]
    Just nbrs -> Just (nbr:nbrs)

addToGraph :: (Node, [Node]) -> M.Map Node [Node] -> M.Map Node [Node]
addToGraph (_, []) adj = adj
addToGraph (nbr, x:xs) adj =
    addToGraph (nbr, xs) $ M.alter (updateNbrs nbr) x adj

countNbrs :: S.Set Node -> [Node] -> M.Map Node [Node] -> Int
countNbrs nbrs [] _ = length nbrs-1  -- don't include root
countNbrs nbrs (n:ns) adj = case M.lookup n adj of
    Nothing -> countNbrs nbrs' ns adj
    Just ns' -> countNbrs nbrs' (ns++ns') adj
  where
    nbrs' = S.insert n nbrs

main = interact $ show
    .countNbrs S.empty [("shiny","gold")]
    .foldr addToGraph M.empty
    .map parse.lines

-}

parse :: String -> (Node, [(Int, Node)])
parse s = (container, contents)
  where
    x:xs = chunksOf 4 $ words s
    container = let [a,b,_,_] = x in (a,b)
    contents = if length (xs!!0) < 4 then []
        else map (\[n,a,b,_]->(read n,(a,b))) xs

countChildren :: Node -> M.Map Node [(Int, Node)] -> Int
countChildren root adj = case adj M.! root of
    [] -> 0
    nbrs -> let f (num,nbr) = num * (countChildren nbr adj) in
        sum (map fst nbrs) + sum (map f nbrs)

main = interact $ show
    .countChildren ("shiny","gold")
    .M.fromList.map parse.lines

