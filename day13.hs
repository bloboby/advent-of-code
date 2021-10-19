import Data.List.Split

getEarliest :: (Int, [Int]) -> Int
getEarliest (dep, buses) = minWait * minId
  where
    (minWait, minId) = foldr f (maximum buses, 0) buses
    f id (accWait, accId) =
        let wait = id - (dep `mod` id)
        in if wait < accWait then (wait, id) else (accWait, accId)

parse :: [String] -> (Int, [Int])
parse [dep, buses] = (read dep, getBuses buses)
    where getBuses = map read.filter (/="x").splitOn ","

-- main = interact $ show.getEarliest.parse.lines


crt :: Int -> Int -> Int -> [String] -> Int
crt val _ _ [] = val
crt val base idx (bus:buses)
    | bus == "x" = crt val base (idx+1) buses
    | otherwise =
        let id = read bus
            eq n = (n+idx) `mod` id == 0
            val' = head $ filter eq [val, val+base ..]
        -- all ids are prime
        in crt val' (base*id) (idx+1) buses


main = interact $ show.crt 0 1 0.splitOn ",".(!!1).lines

