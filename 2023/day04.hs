import Data.List.Split (splitOneOf)
import Data.Set (Set)
import Data.Set qualified as S

type Card = [[Int]] -- [winning, owned]

parseCard :: String -> Card
parseCard = map (map read . words) . tail . splitOneOf ":|"

countWinners :: Card -> Int
countWinners wo =
  let [w, o] = map S.fromList wo
   in length $ w `S.intersection` o

part1 :: [Int] -> Int
part1 xs =
  let score x = if x == 0 then 0 else 2 ^ (x - 1)
   in sum $ map score xs

part2 :: [Int] -> Int
part2 xs =
  let distribute n x ns = map (+ n) (take x ns) ++ drop x ns
      f (acc, ns) x =
        let n = head ns
         in (acc + n, distribute n x (tail ns))
   in fst $ foldl f (0, repeat 1) xs

main = do
  contents <- getContents
  let winners = map (countWinners . parseCard) $ lines contents
  print $ part1 winners
  print $ part2 winners