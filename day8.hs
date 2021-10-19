import qualified Data.IntSet as S

parse :: String -> (String, Int)
parse s = let [exe, (sgn:n)] = words s in
    (exe, if sgn == '+' then read n else -read n)

execute :: Int -> Int -> S.IntSet -> [(String, Int)] -> Int -> Maybe Int
execute pos val vis code err
    | pos == length code = Just val
    | pos `S.member` vis = Nothing
    | otherwise = if exe == "acc"
        then execute (pos+1) (val+n) vis' code err
        else let n' = (if useOne then 1 else n) in
            execute (pos+n') val vis' code err
      where
        (exe, n) = (code!!pos)
        vis' = S.insert pos vis
        useOne = (pos == err) == (exe == "jmp")

executeAll :: [Int] -> [(String, Int)] -> Int
executeAll (err:errs) code = case execute 0 0 S.empty code err of
    Just ans -> ans
    Nothing -> executeAll errs code


main = interact $ show
    .(\c -> executeAll [0..(length c-1)] c)
    .map parse.lines

