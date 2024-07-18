import Data.Array (Array, listArray, (!))
import Data.Char (isDigit)
import Data.List (elemIndex)
import Data.List.Split (chunksOf, splitOn, splitOneOf)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromJust)

type Part = Array Int Int

type Condition = (Int, Char, Int, Bool) -- (xmas, op, val, negate)

type Rule = ([Condition], String)

type Workflows = Map String [Rule]

type Fns = Map String (Part -> String)

parse :: String -> (Workflows, [Part])
parse contents = (workflows, parts)
  where
    [a, b] = splitOn "\n\n" contents
    workflows = M.fromList . map parseWorkflow $ lines a
    parts = map parsePart $ lines b

    parseRule rule
      | ':' `elem` rule =
          let [cond, next] = splitOn ":" rule
              xmas : op : val = cond
              n = fromJust $ elemIndex xmas "xmas"
           in ([(n, op, read val, False)], next)
      | otherwise = ([], rule)

    parseWorkflow x =
      let key : rules : _ = splitOneOf "{}" x
       in (key, map parseRule $ splitOn "," rules)

    parsePart = listArray (0, 3) . map (read . filter isDigit) . splitOn ","

-- Part 1

toFn :: Rule -> (Part -> Maybe String)
toFn ([], next) = const $ Just next
toFn ([(n, op, val, _)], next) =
  let f = if op == '<' then (< val) else (> val)
   in (\x -> if f (x ! n) then Just next else Nothing)

toFns :: Workflows -> Fns
toFns = M.map (go . map toFn)
  where
    go (f : fs) x = case f x of
      Just result -> result
      Nothing -> go fs x

apply :: Fns -> String -> Part -> Bool
apply fns curr part
  | next == "A" = True
  | next == "R" = False
  | otherwise = apply fns next part
  where
    next = (fns M.! curr) part

-- Part 2

propagate :: [Condition] -> [Rule] -> [Rule]
propagate _ [] = []
propagate nots ((cond, x) : xs) =
  let [(a, b, c, _)] = cond
      nots' = (a, b, c, True) : nots
   in (nots ++ cond, x) : propagate nots' xs

dfs :: Workflows -> [Condition] -> String -> [[Condition]]
dfs workflows conds curr
  | curr == "A" = [conds]
  | curr == "R" = []
  | otherwise =
      let rules = workflows M.! curr
          go (c, x) = dfs workflows (conds ++ c) x
       in concatMap go rules

updateKV :: Condition -> (Int, Int, Int -> Int -> Int)
updateKV (xmas, op, val, negate)
  | (op, negate) == ('<', False) = (2 * xmas + 1, val - 1, min)
  | (op, negate) == ('>', False) = (2 * xmas, val + 1, max)
  | (op, negate) == ('<', True) = (2 * xmas, val, max)
  | (op, negate) == ('>', True) = (2 * xmas + 1, val, min)

count :: [Condition] -> Int
count conds =
  let start = M.fromList $ zip [0 .. 7] (cycle [1, 4000])
      f bounds cond =
        let (k, v, dv) = updateKV cond
         in M.insertWith dv k v bounds
      ranges = chunksOf 2 . M.elems $ foldl f start conds
   in product $ map (\[a, b] -> b - a + 1) ranges

main :: IO ()
main = do
  contents <- getContents
  let (workflows, parts) = parse contents
      fns = toFns workflows
      part1 = sum . map sum $ filter (apply fns "in") parts

      propagated = M.map (propagate []) workflows
      ranges = dfs propagated [] "in"
      part2 = sum $ map count ranges

  print part1
  print part2
