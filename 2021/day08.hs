import qualified Data.Map as M
import qualified Data.Set as S
import Data.Tuple (swap)
import Util

type Key = M.Map Char Char

getKey :: [String] -> Key
getKey input = M.fromList $ zip [a,b,c,d,e,f,g] "abcdefg"
 where
  count k = M.insertWith (+) k 1
  counter = foldr count M.empty $ concat input
  fromFreq c f = (M.fromList . map swap $ M.toList c) M.! f
  [b,e,f] = map (fromFreq counter) [6,4,9]

  len = M.fromList $ zip (map length input) input
  fromLen l = S.fromList $ len M.! l
  [n1,n4,n7] = map fromLen [2,4,3]
  a' = S.difference n7 n1
  d' = S.difference (S.difference n4 n1) (S.singleton b)
  [a,d] = map (head . S.toList) [a',d']

  counter' = M.filterWithKey (\k _ -> k /= a && k /= d) counter
  [c,g] = map (fromFreq counter') [8,7]

toInt :: Key -> String -> Int
toInt key encoded = display M.! decoded
  where decoded = S.fromList $ map (key M.!) encoded
        nums = ["abcefg", "cf", "acdeg", "acdfg", "bcdf", "abdfg",
                "abdefg", "acf", "abcdefg", "abcdfg"]
        display = M.fromList $ zip (map S.fromList nums) [0..]

decode :: String -> Int
decode l =  foldl (\acc n -> 10*acc+n) 0 digits
  where key = getKey . take 10 $ words l
        digits = map (toInt key) . drop 11 $ words l

main = interact $ Util.print . sum . map decode . lines
