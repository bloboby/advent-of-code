
step :: Int -> Int -> Int
step subj val = (val*subj) `rem` 20201227

findSize :: Int -> Int -> Int -> Int
findSize count val targetVal
    | val == targetVal = count
    | otherwise = findSize (count+1) (step 7 val) targetVal

transform :: Int -> Int -> Int -> Int
transform size subj val = (!!size) $ iterate (step subj) 1


main = do
    contents <- getContents
    let [keyA, keyB] = map read.filter (not.null) $ lines contents
    let [sizeA, sizeB] = map (findSize 0 1) [keyA, keyB]
    putStrLn.show $ (sizeA, sizeB)
    putStrLn.show $ transform sizeA keyB 1
    putStrLn.show $ transform sizeB keyA 1

