import qualified Data.Set as S
import Data.Set (Set)

parse contents =
  let unroll [[dir], n] = take (read n) $ repeat dir
  in concatMap unroll . map words $ lines contents

move (x,y) dir = case dir of
  'U' -> (x-1, y)
  'D' -> (x+1, y)
  'L' -> (x, y-1)
  'R' -> (x, y+1)

catchup (hx,hy) (tx,ty)
  | dx <= 1 && dy <= 1 = (tx, ty)
  | dx > dy = (hx', hy)
  | dy > dx = (hx, hy')
  | otherwise = (hx', hy')
  where [dx, dy] = map abs [hx-tx, hy-ty]
        [hx', hy'] = [hx + signum (tx-hx), hy + signum (ty-hy)]

propagate t (prev, ts') =
  let t' = catchup prev t
  in (t', t':ts')

doMove (h, ts, vis) m =
  let h' = move h m
      (t, ts') = foldr propagate (h', []) ts
  in (h', ts', S.insert t vis)

solve moves =
  let ts = take 9 $ repeat (0,0)  -- take 1 for part 1
      (_, _, vis) = foldl doMove ((0,0), ts, S.singleton (0,0)) moves
  in length vis

main = do
  contents <- getContents
  let moves = parse contents
  print $ solve moves
