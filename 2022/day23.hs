import qualified Data.Map as M
import qualified Data.Set as S
import Data.Set (Set)

type Elf = [Int]

parse :: String -> Set Elf
parse s = S.fromList . map fst . filter ((=='#') . snd) $
  [([i,j], x) | (i, row) <- zip [0..] $ lines s,
                (j, x) <- zip [0..] row]

getAllNbrs elf = map (zipWith (+) elf) $
  [[-1,-1], [-1,0], [-1,1], [0,-1], [0,1], [1,-1], [1,0], [1,1]]

getNbrs elf dir = map (zipWith (+) elf) $ case dir of
  'N' -> [[-1,-1], [-1,0], [-1,1]]
  'E' -> [[-1,1], [0,1], [1,1]]
  'S' -> [[1,-1], [1,0], [1,1]]
  'W' -> [[-1,-1], [0,-1], [1,-1]]

propose elves dirs elf
  | noElfIn $ getAllNbrs elf = elf
  | noElfIn n1 = e1
  | noElfIn n2 = e2
  | noElfIn n3 = e3
  | noElfIn n4 = e4
  | otherwise = elf
  where noElfIn = not . any (`S.member` elves)
        nbrs@[n1,n2,n3,n4] = map (getNbrs elf) dirs
        [e1,e2,e3,e4] = map (!!1) nbrs

doRound :: (Set Elf, [Char]) -> (Set Elf, [Char])
doRound (elves, dirs@[a,b,c,d]) =
  let f acc e = M.insertWith (++) (propose elves dirs e) [e] acc
      counts = S.foldl' f M.empty elves
      move p es acc = case es of
                        [e] -> S.insert p acc
                        _   -> foldr S.insert acc es
      elves' = M.foldrWithKey move S.empty counts
  in (elves', [b,c,d,a])

simulate elves dirs n
  | elves' == elves = n
  | otherwise = simulate elves' dirs' (n+1)
  where (elves', dirs') = doRound (elves, dirs)

solve1 elves =
  let final = fst . (!!10) $ iterate doRound (elves, "NSWE")
      [xmin, xmax] = map ($ S.map head final) [minimum, maximum]
      [ymin, ymax] = map ($ S.map last final) [minimum, maximum]
  in (xmax - xmin + 1) * (ymax - ymin + 1) - S.size elves

solve2 elves = simulate elves "NSWE" 1

main = do
  contents <- getContents
  let elves = parse contents
  print $ solve1 elves
  print $ solve2 elves
