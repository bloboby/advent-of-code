import qualified Data.IntSet as S
import qualified Data.Map as M

solve :: S.IntSet -> [Int] -> Int
solve s (n:ns) = if (2020-n) `S.member` s
    then n*(2020-n)
    else solve (S.insert n s) ns

-- main = interact $ show.solve S.empty.map read.words

solve' :: [Int] -> Int
solve' ns = findTriple ns
  where
    findTriple (m:ms) = case M.lookup (2020-m) pairs of
        Just (x,y) -> m*x*y
        Nothing -> findTriple ms
    pairs = M.fromList [(n+m, (n,m)) | n <- ns, m <- ns]

main = interact $ show.solve'.map read.words

