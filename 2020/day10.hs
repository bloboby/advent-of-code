import Data.List  -- sort

countJolts :: Int -> Int -> Int -> [Int] -> Int
countJolts oneJ threeJ _ [] = oneJ * (threeJ + 1)
countJolts oneJ threeJ j (n:ns)
    | n == j+1 = countJolts (oneJ+1) threeJ n ns
    | n == j+3 = countJolts oneJ (threeJ+1) n ns
    | otherwise = countJolts oneJ threeJ n ns

-- main = interact $ show.countJolts 0 0 0.sort.map read.lines

countArrs :: [(Int, Int)] -> [Int] -> Int
countArrs dp [] = snd $ last dp
countArrs dp jolts@(j:js)
    | fst (head dp) < j-3 = countArrs (tail dp) jolts
    | otherwise = let a = sum $ map snd dp in
        countArrs (dp ++ [(j,a)]) js

main = interact $ show
    .countArrs [(0,1)]
    .(\a -> a ++ [last a + 3])
    .sort.map read.lines

