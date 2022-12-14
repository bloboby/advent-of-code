import qualified Data.Set as S
import Data.List.Split (splitOn)
import Data.Set (Set)

type Rocks = Set (Int, Int)

drawLine ([x1, y1], [x2, y2])
  | x1 == x2 = zip (repeat x1) [y1, y1+dy .. y2]
  | y1 == y2 = zip [x1, x1+dx .. x2] (repeat y1)
  where dx = signum $ x2 - x1
        dy = signum $ y2 - y1

getRocks :: String -> [(Int, Int)]
getRocks l = let corners = map (map read . splitOn ",") . splitOn " -> " $ l
             in concat . map drawLine . zip corners $ tail corners

resolve1 :: (Int, Int) -> Int -> Rocks -> Maybe Rocks
resolve1 (x,y) maxY rocks
  | y > maxY = Nothing
  | not $ (x,y+1) `S.member` rocks = resolve1 (x,y+1) maxY rocks
  | not $ (x-1,y+1) `S.member` rocks = resolve1 (x-1,y+1) maxY rocks
  | not $ (x+1,y+1) `S.member` rocks = resolve1 (x+1,y+1) maxY rocks
  | otherwise = Just $ S.insert (x,y) rocks

resolve2 (x,y) maxY rocks
  | (x,y) `S.member` rocks = Nothing
  | y == maxY + 1 = rocks'
  | not $ (x,y+1) `S.member` rocks = resolve2 (x,y+1) maxY rocks
  | not $ (x-1,y+1) `S.member` rocks = resolve2 (x-1,y+1) maxY rocks
  | not $ (x+1,y+1) `S.member` rocks = resolve2 (x+1,y+1) maxY rocks
  | otherwise = rocks'
  where rocks' = Just $ S.insert (x,y) rocks

pourSand :: Int -> Int -> Rocks -> Int
pourSand maxY n rocks =
  case resolve2 (500,0) maxY rocks of
    Just rocks' -> pourSand maxY (n+1) rocks'
    Nothing -> n

main = do
  contents <- getContents
  let rocks = concat . map getRocks . lines $ contents
      maxY = maximum . map snd $ rocks
      ans = pourSand maxY 0 $ S.fromList rocks
  print ans
