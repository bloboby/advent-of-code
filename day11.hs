import qualified Data.Sequence as Seq
import Data.Char (isDigit)
import Data.List (sort)
import Data.List.Split (splitOn)
import Data.Sequence (Seq(..), (|>))

type Action = Int -> (Int, Int)  -- val -> (id, val')
type Items = Seq Int
type State = (Seq Items, Seq Int)

-- parsing

getOp op x
  | (op, x) == ("*", "old") = (^2)
  | op == "+" = (+n)
  | op == "*" = (*n)
  where n = read x

getAction [_, _, _, _, op, x] [test, true, false] val =
  -- let val' = div (getOp op x val) 3
  let val' = mod (getOp op x val) 9699690  -- hardcoded CRT
  in if mod val' test == 0 then (true, val') else (false, val')

parseMonkey :: String -> (Action, Items)
parseMonkey s =
  let [_,b,c,d,e,f] = lines s
      action = getAction (words c) $ map (read . last . words) [d, e, f]
      items = Seq.fromList . map (read . filter isDigit) . tail . tail $ words b
  in (action, items)

-- monkey business

redistribute s (k,v) = Seq.adjust (\vs -> vs|>v) k s

doRound :: [Int] -> Seq Action -> State -> State
doRound [] _ state = state
doRound (id:ids) actions (items, counts) =
  let xs = Seq.index items id
      thrown = fmap (Seq.index actions id) xs
      items' = foldl redistribute (Seq.update id Seq.empty items) thrown
      counts' = Seq.adjust (+ length xs) id counts
  in doRound ids actions (items', counts')

main = do
  contents <- getContents
  let monkeys = map parseMonkey $ splitOn "\n\n" contents
      n = length monkeys
      actions = Seq.fromList . map fst $ monkeys
      start = (Seq.fromList . map snd $ monkeys, Seq.fromList . take n $ repeat 0)
      counts = snd $ iterate (doRound [0..n-1] actions) start !! 10000
      (_:|>a:|>b) = Seq.sort counts
  print (a*b, counts)
