import qualified Data.Set as S
import Util

type Herd = S.Set [Int]

preprocess :: String -> ([Herd], [Int])
preprocess input = (herds, bounds)
  where kv = [([x,y],v) | (x,row) <- zip [0..] $ lines input,
                          (y,v) <- zip [0..] row]
        getHerd c = S.fromList . map fst . filter ((==c).snd) $ kv
        herds = map getHerd ['>', 'v']
        bounds = fst . maximum $ kv

move :: [Int] -> [Herd] -> [Herd]
move [n,m] [east, south] =
  let all = S.union east south
      all' = S.union east' south
      east' = let valid cu = e cu `S.notMember` all
              in foldr moveEast east $ S.filter valid east
      south' = let valid cu = s cu `S.notMember` all'
               in foldr moveSouth south $ S.filter valid south
  in [east', south']
  where e [x,y] = [x, (y+1) `mod` (m+1)]
        s [x,y] = [(x+1) `mod` (n+1), y]
        moveEast cu acc = S.insert (e cu) . S.delete cu $ acc
        moveSouth cu acc = S.insert (s cu) . S.delete cu $ acc

moveAll :: [Int] -> [Herd] -> Int -> Int
moveAll bounds herds count
  | herds' == herds = (count+1)
  | otherwise = moveAll bounds herds' (count+1)
  where herds' = move bounds herds

solve (herds, bounds) = moveAll bounds herds 0

main = interact $ Util.print . solve . preprocess
