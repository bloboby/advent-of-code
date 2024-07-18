import qualified Data.Map as M
import Data.List.Split (splitOn)
import Util

type Counter = M.Map Int Int

preprocess :: String -> Counter
preprocess s = foldr increment M.empty list
  where increment k = M.insertWith (+) k 1
        list = map read . splitOn "," . head . lines $ s

solve :: Counter -> Int
solve counter = minimum $ zipWith (+) lf rf
  where [l,r] = map (fst.($ counter)) [M.findMin, M.findMax]
        addCrabs n pos = (+) n $ M.findWithDefault 0 pos counter
        getFuel = init . scanl1 (+) . scanl1 (+) . scanl addCrabs 0
        [lf, rf] = map ($ [l..r]) [getFuel, reverse.getFuel.reverse]

main = interact $ Util.print . solve . preprocess
