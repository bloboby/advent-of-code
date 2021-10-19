import Data.List
import Data.List.Split
import Data.Ord
import qualified Data.Map as M
import qualified Data.Set as S

-- parse and filter out invalid tickets

preprocess :: String -> (M.Map String [(Int,Int)], [Int], [[Int]])
preprocess s = (rules, myTicket, tickets)
  where
    (rawRules:rawMyTicket:rawTickets:_) = splitOn "\n\n" s
    rules = M.fromList.map parseRule.lines $ rawRules
    myTicket = map read.splitOn ",".last.lines $ rawMyTicket
    tickets = filter (isValid.concat.M.elems $ rules)
        .map (map read.splitOn ",")
        .filter (not.null).tail.lines $ rawTickets

parseRule :: String -> (String, [(Int,Int)])
parseRule s = (name, ranges)
  where
    [name, rawRanges] = splitOn ": " s
    ranges = map ((\[x,y]->(x,y)).map read.splitOn "-")
        .splitOn " or " $ rawRanges

isValid :: [(Int,Int)] -> [Int] -> Bool
isValid ranges ticket = foldr f True ticket
  where
    f v acc = if not $ foldr (g v) False ranges then False else acc
    g v (x,y) acc' = if (x <= v) && (v <= y) then True else acc'

-- filter options by ticket

filterByTicket :: M.Map String [(Int,Int)] -> [[Int]] -> [S.Set String]
filterByTicket rules tickets =
    foldr (pare rules) options tickets
  where
    options = take (M.size rules) names
    names = repeat.S.fromList.M.keys $ rules

pare rules ticket options = options'
  where
    options' = map updateOptions $ zip ticket options
    updateOptions (v, names) = S.filter (filterByVal v) names
    filterByVal v = or.map (inRange v).(rules M.!)
    inRange v (x,y) = (x <= v) && (v <= y)

-- filter options by uniqueness

getPositions :: M.Map String Int -> [(Int, S.Set String)]
    -> M.Map String Int
getPositions positions [] = positions
getPositions positions ((i,x):xs) =
    getPositions positions' xs'
  where
    -- assumes that there is a unique solution, so x is a singleton
    name = S.findMin x
    positions' = M.insert name i positions
    xs' = map (\(i',names) -> (i', S.delete name names)) xs

-- extract the answer

multiplyPos :: [Int] -> (String, Int) -> Int -> Int
multiplyPos myTicket (name, pos) ans =
    if (head $ words name) == "departure"
        then ans * (myTicket!!pos) else ans


main = do
    contents <- getContents
    let (rules, myTicket, tickets) = preprocess contents
    let positions = M.toList.getPositions M.empty
          .sortBy (comparing (S.size.snd)).zip [0..]
          $ filterByTicket rules tickets
    let ans = foldr (multiplyPos myTicket) 1 positions
    putStrLn $ show ans

