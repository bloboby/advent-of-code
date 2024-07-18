import Data.List.Split
import qualified Data.Set as S

type Deck = [Int]
type History = S.Set (Deck, Deck)
data Winner = P1 | P2


play :: Deck -> Deck -> History -> (Winner, Deck)
play [] q _ = (P2, q)
play q [] _ = (P1, q)
play qa@(a:as) qb@(b:bs) history
    | (qa, qb) `S.member` history = (P1, qa)
    | length as < a || length bs < b =
        if a>b then play as' bs history'
        else play as bs' history'
    | otherwise = case play (take a as) (take b bs) S.empty of
        (P1, q) -> play as' bs history'
        (P2, q) -> play as bs' history'
  where
    as' = (as++[a]++[b])
    bs' = (bs++[b]++[a])
    history' = S.insert (qa, qb) history

score :: Deck -> Int
score q = sum.zipWith (*) [1..].reverse $ q


main = do
  contents <- getContents
  let [a,b] = map (map read.drop 2.words).filter (not.null)
        $ splitOn ("\n\n") contents
  let (_, q) = play a b S.empty
  putStrLn.show $ score q

