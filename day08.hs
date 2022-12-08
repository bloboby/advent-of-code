import qualified Data.Map as M
import Data.Map (Map)

type Grid = Map (Int, Int) Char
type 

parseGrid :: [[Char]] -> Grid
parseGrid rows = M.fromList $
  [((i,j),x) | (i,row) <- zip [0..] rows,
               (j,x) <- zip [0..] row]

-- store a set of visible coords

main = do
  contents <- getContents
  let rows = lines contents
      (n, m) = (length rows, length $ head rows)
  let x = parseGrid rows
  print x
  print (n,m)
