import qualified Data.Map as M
import qualified Data.Sequence as S
import Data.Map (Map, (!))
import Data.Sequence (Seq(..))

target = 14  -- use 4 for part 1

dec k m = let v = m ! k
          in if v == 1 then M.delete k m else M.insert k (v-1) m

findMarker :: Int -> Map Char Int -> Seq Char -> [Char] -> Int
findMarker n hist queue (x:xs)
  | M.size hist == target = n
  | length queue < target = findMarker (n+1) hist' (queue:|>x) xs
  | otherwise = let (q:<|qs) = queue
                in findMarker (n+1) (dec q hist') (qs:|>x) xs
  where hist' = M.insertWith(+) x 1 hist

main = do
  contents <- getContents
  print . map (findMarker 0 M.empty S.empty) $ lines contents
