import Data.List.Split (splitOn)
import Data.Map (Map)
import Data.Map qualified as M

type Nav = Map String (String, String)

parseLine :: String -> (String, (String, String))
parseLine x =
  let [a, _, b, c] = words x
   in (a, (tail (init b), init c))

parse :: String -> (String, Nav)
parse x =
  let [a, b] = splitOn "\n\n" x
      parseNav = M.fromList . map parseLine . lines
   in (cycle a, parseNav b)

loopSize :: String -> Nav -> (String -> Bool) -> Int -> String -> Int
loopSize (x : xs) nav end steps curr
  | end curr = steps
  | otherwise =
      let f = if x == 'L' then fst else snd
          curr' = f $ nav M.! curr
       in loopSize xs nav end (steps + 1) curr'

main = do
  contents <- getContents
  let (instr, nav) = parse contents
      part1 = loopSize instr nav (== "ZZZ") 0 "AAA"

      starts = filter ((== 'A') . last) (M.keys nav)
      loops = map (loopSize instr nav ((== 'Z') . last) 0) starts
      part2 = foldr lcm 1 loops

  print part1
  print part2