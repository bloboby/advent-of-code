import qualified Data.Map as M
import qualified Data.Set as S
import Data.List.Split (chunksOf)
import Data.Map (Map)
import Data.Set (Set)

data Cmd = Noop | Add Int deriving Show
type Acc = (Int, Int, Map Int Int)

parseLine line =
  if line =="noop" then Noop
  else let [_, n] = words line in Add (read n)

foldHist :: Acc -> Cmd -> Acc
foldHist (t, v, hist) Noop = (t+1, v, M.insert (t+1) v hist)
foldHist (t, v, hist) (Add n) =
  let hist' = M.insert (t+1) v . M.insert (t+2) (v+n) $ hist
  in (t+2, v+n, hist')

pixel t n = if abs (n - mod (t-1) 40) <= 1 then '#' else '.'

main = do
  contents <- getContents
  let cmds = map parseLine $ lines contents
      (_,_,hist) = foldl foldHist (1, 1, M.singleton 1 1) cmds
      part1 = sum $ map (\t -> t * hist M.! t) [20,60..220]
      part2 = map (map (\t -> pixel t $ hist M.! t)) $ chunksOf 40 [1..240]
  print part1
  putStrLn $ unlines part2
