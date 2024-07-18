import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char (isLower)
import Data.List.Split (splitOn)
import Util

type Graph = M.Map String [String]

makeGraph :: String -> Graph
makeGraph input = foldr addEdge M.empty edges
  where edges = map (splitOn "-") $ lines input
        addEdge [a,b] = M.insertWith (++) a [b] 
                      . M.insertWith (++) b [a]

explore :: Graph -> S.Set String -> Bool -> String -> Int
explore g vis doubled node
  | node == "end" = 1
  | isLower (head node) && node `S.member` vis =
      if doubled || node == "start" then 0
      else sum . map (explore g vis' True) $ g M.! node
  | otherwise = sum . map (explore g vis' doubled) $ g M.! node
  where vis' = S.insert node vis

solve g = explore g S.empty False "start"

main = interact $ Util.print . solve . makeGraph
