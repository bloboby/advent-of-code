import Data.Char
import Data.List
import Data.List.Split
import Data.Ord
import qualified Data.Map as M
import qualified Data.Set as S

type Item = String
type Agn = String
type Counts = M.Map Item Int
type Chart = M.Map Agn (S.Set Item)


parse :: String -> ([Item], [Agn])
parse s = (words lhs, words rhs)
  where
    [lhs, rhs] = splitOn(" contains ") $ filter notSymbol s
    notSymbol c = isAlpha c || isSpace c

addItems :: ([Item], [Agn]) -> Counts -> Counts
addItems (items, _) acc = foldr addItem acc items
  where
    addItem x = M.insert x (countItem x + 1)
    countItem x = if x `M.member` acc then acc M.! x else 0

updateAgns :: ([Item], [Agn]) -> Chart -> Chart
updateAgns (items, agns) acc = foldr updateAgn acc agns
  where
    updateAgn agn acc' = let x = S.fromList items in
        case M.lookup agn acc' of
            Just x' -> M.insert agn (S.intersection x' x) acc'
            Nothing -> M.insert agn x acc'

-- part 1

countSafe :: Counts -> Chart -> Int
countSafe counts chart = sum.map countIfSafe $ M.toList counts
  where
    countIfSafe (x,n) = if x `S.member` unsafe then 0 else n
    unsafe = foldr1 S.union $ M.elems chart

-- part 2

getDangerous :: Chart -> String
getDangerous chart = getPairs chart' []
  where
    chart' = sortBy (comparing $ S.size.snd) $ M.toList chart

getPairs :: [(Agn, S.Set Item)] -> [(Agn, Item)] -> String
getPairs [] p = intercalate ",".map snd $ sortBy (comparing fst) p
getPairs ((agn, items):xs) pairs = getPairs xs' pairs'
  where
    xs' = map (\(a,x) -> (a, S.delete item x)) xs
    pairs' = (agn, item):pairs
    item = S.findMin items  -- items is a singleton


main = do
    contents <- getContents
    let input = map parse.filter (not.null).lines $ contents
    let counts = foldr addItems M.empty input
    let chart = foldr updateAgns M.empty input
    -- part 1
    putStrLn.show $ countSafe counts chart
    -- part 2
    putStrLn.show $ getDangerous chart

