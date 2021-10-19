import qualified Data.IntSet as S


findInvalid :: ([Int], [Int]) -> Int
findInvalid (preamble, (x:xs))
    | not $ isSum S.empty preamble x = x
    | otherwise = findInvalid ((tail preamble ++ [x]), xs)

isSum :: S.IntSet -> [Int] -> Int -> Bool
isSum _ [] _ = False
isSum diffs (p:ps) x
    | p `S.member` diffs && x /= 2*p = True
    | otherwise = isSum (S.insert (x-p) diffs) ps x

findChain :: [Int] -> Int -> [Int] -> Int -> Int
findChain chain sum (x:xs) target
    | (sum == target) && (length chain >= 2)
        = (minimum chain) + (maximum chain)
    | (sum <= target) || (length chain < 2)
        = findChain (x:chain) (sum+x) xs target
    | otherwise
        = findChain (init chain) (sum-last chain) (x:xs) target

main = do
    contents <- getContents
    let input = map read $ lines contents
    let invalid = findInvalid (take 25 input, drop 25 input)
    putStrLn.show $ findChain [] 0 input invalid

