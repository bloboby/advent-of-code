import qualified Data.Map as M
import Data.Map (Map)

import Debug.Trace

data Tree = Dir String (Map String Tree) | File String Int deriving Show
type Context = [(String, Tree)]

reconstruct :: Tree -> Context -> [[String]] -> Tree
reconstruct t _ [] = t
reconstruct _ _ (["$", "cd", "/"]:lines) = reconstruct (Dir "/" M.empty) [] lines
reconstruct (Dir name children) context (line:lines)
  | last line == ".."
    =  undefined
  | a == "$" && b == "cd" =
      
  | a == "$" && b == "ls" = reconstruct (Dir name children) context lines
  | a == "dir" =
      let children' = M.insert b (Dir b M.empty) children
      in reconstruct (Dir name children') context lines
  | otherwise =
      let children' = M.insert b (File b (read a)) children
      in reconstruct (Dir name children') context lines
  where (a:b:_) = line

main = do
  contents <- getContents
  let x = map words $ lines contents
  print x
