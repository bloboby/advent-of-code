import qualified Data.Map as M
import qualified Data.Sequence as Seq
import Data.Map (Map)
import Data.Sequence (Seq(..))

import Data.Char (isDigit)
import Data.List (sort)
import Data.List.Split (splitOn)

type Monkey = ((Int -> (Int, Int)), Seq Int)  -- val -> (id, val')
type Monkeys = Map Int Monkey
type Counts = Map Int Int

-- parsing

getOp op x
  | (op, x) == ("*", "old") = (^2)
  | op == "+" = (+n)
  | op == "*" = (*n)
  where n = read x

getAction [_, _, _, _, op, x] [test, true, false] val =
  -- For part 1, let val' = div (getOp op x val) 3.
  let val' = mod (getOp op x val) 9699690  -- hardcoded CRT
  in if mod val' test == 0 then (true, val') else (false, val')

parseMonkey :: String -> (Int, Monkey)
parseMonkey s =
  let [a,b,c,d,e,f] = lines s
      id = read $ filter isDigit a
      action = getAction (words c) $ map (read . last . words) [d, e, f]
      items = Seq.fromList . map (read . filter isDigit) . tail . tail $ words b
  in (id, (action, items))

-- monkey business

doTurn :: Monkey -> Seq (Int, Int)
doTurn (_, Empty) = Seq.empty
doTurn (f, (x:<|xs)) = (f x):<|(doTurn (f, xs))

redistribute :: Seq (Int, Int) -> Monkeys -> Monkeys
redistribute Empty ms = ms
redistribute ((k, v):<|kvs) ms =
  let (f, xs) = ms M.! k
      ms' = M.insert k (f, xs:|>v) ms
  in redistribute kvs ms'

doRound :: [Int] -> (Monkeys, Counts) -> (Monkeys, Counts)
doRound [] state = state
doRound (id:ids) (ms, counts) =
  let m@(f,xs) = ms M.! id
      ms' = redistribute (doTurn m) $ M.insert id (f, Seq.empty) ms
      counts' = M.insertWith (+) id (length xs) counts
  in doRound ids (ms', counts')

main = do
  contents <- getContents
  let monkeys = M.fromList . map parseMonkey $ splitOn "\n\n" contents
      ids = M.keys monkeys
      counts = snd $ (iterate (doRound ids) (monkeys, M.empty)) !! 10000
      (a:b:_) = reverse . sort . M.elems $ counts
  print $ a*b
