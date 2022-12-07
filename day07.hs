import qualified Data.Map as M
import Data.List (sort)
import Data.Map (Map)

data Tree = Dir String (Map String Tree) | File String Int deriving Show
type Ancestors = [Tree]

-- reconstruct the filesystem

reconstruct :: Tree -> Ancestors -> [[String]] -> Tree
reconstruct t anc [] = reroot t anc

reconstruct t@(Dir name _) ((Dir parent sibs):anc) (["$","cd",".."]:xs) =
  let sibs' = M.insert name t sibs
  in reconstruct (Dir parent sibs') anc xs

reconstruct t@(Dir _ children) anc (["$","cd",x]:xs) =
  let Dir _ t' = children M.! x
  in reconstruct (Dir x t') (t:anc) xs

reconstruct (Dir name children) anc ([x,y]:xs) =
  let child = if x == "dir" then Dir y M.empty else File y (read x)
      children' = M.insert y child children -- assumes y doesn't already exist!
  in reconstruct (Dir name children') anc xs

reroot t [] = t
reroot t@(Dir name _) ((Dir parent sibs):anc) =
  let sibs' = M.insert name t sibs
  in reroot (Dir parent sibs') anc

-- other

getSizes :: Tree -> ([Int], [Int])  -- (dirs, files)
getSizes (File _ n) = ([], [n])
getSizes (Dir _ m) =
  let concat (a, b) (c, d) = (a++c, b++d)
      (dirs, files) = M.foldr (\t acc -> concat (getSizes t) acc) ([],[]) m
      curr = sum files
  in (curr:dirs, files)

main = do
  contents <- getContents
  let input = map words . tail . filter (/="$ ls") . lines $ contents
      dirSizes = fst . getSizes $ reconstruct (Dir "/" M.empty) [] input
      part1 = sum . filter (<= 100000) $ dirSizes
      part2 = head . sort . filter (>= head dirSizes - 40000000) $ dirSizes
  print (part1, part2)
