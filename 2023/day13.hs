import Data.List (find, transpose)
import Data.List.Split (splitOn)
import Data.Maybe (fromJust, isJust)

kSmudges :: Int = 1

dist :: String -> String -> Int
dist x y = length . filter id $ zipWith (/=) x y

reflect :: [String] -> Maybe Int
reflect rows =
  let adj = zipWith (\x y -> 1 >= dist x y) rows (tail rows)
      candidates = map fst . filter snd $ zip [1 ..] adj
   in find (verify rows) candidates

verify :: [String] -> Int -> Bool
verify rows x =
  let l = reverse $ take x rows
      r = drop x rows
   in (== kSmudges) . sum $ zipWith dist l r

summary :: [String] -> Int
summary rows
  | isJust r = 100 * fromJust r
  | isJust c = fromJust c
  where
    r = reflect rows
    c = reflect $ transpose rows

main = do
  contents <- getContents
  let patterns = map lines $ splitOn "\n\n" contents
      ans = sum $ map summary patterns
  print ans