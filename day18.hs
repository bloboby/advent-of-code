import Control.Applicative
import Data.Char (isDigit)
import Data.Maybe (fromJust, isJust)
import Util

data SNum = Regular Int | Pair SNum SNum deriving (Eq, Show)
data Direction = L | R

-- makeTree

accDepth depth c = case c of
  '[' -> depth + 1
  ']' -> depth - 1
  num -> depth

makeTree :: String -> SNum
makeTree s =
  if and $ map isDigit s then Regular $ read s
  else let d = zip s . tail $ scanl accDepth 0 s
           d' = map (\(x,n) -> (x,n-1)) d
           left = map fst . takeWhile (/=(',',0)) . tail $ d'
           right = map fst . tail . dropWhile (/=(',',0)) . init $ d'
       in Pair (makeTree left) (makeTree right)

-- reduce

reduce :: SNum -> SNum
reduce snum = fromJust $ e <|> s <|> Just snum
  where e = reduce . snd <$> tryExplode 0 snum
        s = reduce <$> trySplit snum

tryExplode :: Int -> SNum -> Maybe ([Int], SNum)
tryExplode _ (Regular _) = Nothing
tryExplode 4 (Pair (Regular l) (Regular r)) = Just ([l,r], Regular 0)
tryExplode depth (Pair l r) = explodeL <|> explodeR
 where
  explodeL =
    let cleanup ([dl, dr], l') = ([dl, 0], Pair l' (propagate R dr r))
    in cleanup <$> tryExplode (depth+1) l
  explodeR =
    let cleanup ([dl, dr], r') = ([0, dr], Pair (propagate L dl l) r')
    in cleanup <$> tryExplode (depth+1) r

propagate _ 0 snum = snum
propagate _ diff (Regular n) = Regular (n+diff)
propagate L diff (Pair l r) = Pair l (propagate L diff r)
propagate R diff (Pair l r) = Pair (propagate R diff l) r

trySplit :: SNum -> Maybe SNum
trySplit (Regular n) =
  if n < 10 then Nothing
  else let (l, r) = (n `div` 2, n - l)
       in Just $ Pair (Regular l) (Regular r)
trySplit (Pair l r) = l' <|> r'
  where l' = Pair <$> trySplit l <*> Just r
        r' = Pair <$> Just l <*> trySplit r

-- solve

getMagnitude (Regular n) = n
getMagnitude (Pair l r) = 3 * (getMagnitude l) + 2 * (getMagnitude r)

solve snums = maximum $ map (getMagnitude . add) pairs
  where add (x,y) = reduce $ Pair x y
        pairs = [(x,y) | x <- snums, y <- snums, x /= y]

main = interact $ Util.print . solve . map makeTree . lines
