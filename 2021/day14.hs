import qualified Data.Map as M
import Data.List.Split (splitOn)
import Data.List (sort)
import Util

type Rules = M.Map String [String]
type Polymer = M.Map String Int

inc n m k = M.insertWith (+) k n m

preprocess :: String -> (String, Polymer, Rules)
preprocess s = (t, foldl (inc 1) M.empty template, M.fromList rules)
  where [t, r] = splitOn "\n\n" s
        template = map (\(a,b) -> [a,b]) . zip t $ drop 1 t
        makeRule [[a,c],_,[b]] = ([a,c], [[a,b],[b,c]])
        rules = map (makeRule . words) $ lines r

evolve :: Rules -> Polymer -> Polymer
evolve rules polymer = foldl addPair M.empty $ M.toList polymer
  where addPair acc (pair, n) = if pair `M.notMember` rules
          then inc n acc pair else foldl (inc n) acc $ rules M.! pair

solve (t, template, rules) = (last cts - head cts) `div` 2
  where polymer = iterate (evolve rules) template !! 40
        ends = M.fromList [(head t, 1), (last t, 1)]
        splitPair acc (pair,n) = foldl (inc n) acc pair
        cts = sort . M.elems . foldl splitPair ends $ M.toList polymer

main = interact $ Util.print . solve . preprocess
