import Data.List (group, sort, sortBy)
import Data.Ord (Down (Down), comparing)

data Hand = Hand [Int] Int deriving (Show)

instance Eq Hand where
  (Hand cards1 _) == (Hand cards2 _) = cards1 == cards2

instance Ord Hand where
  compare (Hand cards1 _) (Hand cards2 _)
    | hist1 /= hist2 = compare hist1 hist2
    | otherwise = compare cards1 cards2
    where
      [hist1, hist2] = map histogram [cards1, cards2]

kWild :: Int = 0

cardValue :: Char -> Int
cardValue 'A' = 14
cardValue 'K' = 13
cardValue 'Q' = 12
cardValue 'J' = kWild -- Use 11 for part 1.
cardValue 'T' = 10
cardValue x = read [x]

parseHand :: String -> Hand
parseHand x =
  let [cards, bid] = words x
   in Hand (map cardValue cards) (read bid)

histogram :: [Int] -> [Int]
histogram x =
  let gs = group . sort $ x
      g0 = head gs
      (w, gs') = if head g0 == kWild then (length g0, tail gs) else (0, gs)
      hist = sortBy (comparing Down) $ map length gs'
   in if null hist then [w] else (head hist + w) : tail hist

main = do
  contents <- getContents
  let hands = map parseHand $ lines contents
      score rank (Hand _ bid) = rank * bid
      ans = sum $ zipWith score [1 ..] (sort hands)
  print ans