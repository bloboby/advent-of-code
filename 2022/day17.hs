import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (foldl')
import Data.Map (Map)
import Data.Set (Set)

type Rocks = Set (Int, Int)
type Jets = [(Int, Char)]
type State = (Rocks, Int, Jets)
type History = Map (Int, Int, [Int]) [(Int, Int)]  -- (j, r, profile) -> [(r, h)]

r1 = S.fromList [(0,0), (1,0), (2,0), (3,0)]
r2 = S.fromList [(0,1), (1,0), (1,1), (1,2), (2,1)]
r3 = S.fromList [(0,0), (1,0), (2,0), (2,1), (2,2)]
r4 = S.fromList [(0,0), (0,1), (0,2), (0,3)]
r5 = S.fromList [(0,0), (0,1), (1,0), (1,1)]

-- Part 1

move r (x,y) = S.map (\(a,b) -> (a+x,b+y)) r

clash rocks r =
  let xs = S.map fst r
      bad = not . null $ S.intersection r rocks
  in minimum xs < 0 || maximum xs > 6 || bad

settle :: State -> Rocks -> State
settle (rocks, h, (j:js)) r
  | not . null $ S.intersection r'' rocks =
      let h' = max h . maximum $ S.map snd r'
          rocks' = S.union r' rocks
      in (rocks', h', js)
  | otherwise = settle (rocks, h, js) r''
  where tmp = if snd j == '>' then move r (1,0) else move r (-1,0)
        r' = if clash rocks tmp then r else tmp
        r''  = move r' (0,-1)

newRock :: State -> Rocks -> State
newRock s@(_, h, _) r = settle s $ move r (2,h+4)

solve1 contents =
  let floor = S.fromList . zip [0..6] $ repeat 0
      jets = concat . repeat . zip [0..] $ contents
      rocks = take 2022 . concat . repeat $ [r1,r2,r3,r4,r5]
      (_, height, _) = foldl' newRock (floor, 0, jets) rocks
  in height

-- Part 2

findMax rocks x = snd . maximum . S.filter ((==x) . fst) $ rocks

getProfile rocks =
  let hs = map (findMax rocks) [0..6]
      h = minimum hs
  in map (\x -> x-h) hs

validCycle l
  | length l < 3 = False
  | otherwise = let [(a,b), (c,d), (e,f)] = l
                in (a-c) == (c-e) && (b-d) == (d-f)

findRepeats :: State -> History -> [(Int, Rocks)] -> Int
findRepeats s@(rocks, h, js) hist (r:rs)
  | not $ M.null cycles = extrapolate hist cycles
  | otherwise = findRepeats s' hist' rs
  where cycles = M.filter validCycle hist
        curr = (fst $ head js, mod (fst r) 5, getProfile rocks)
        hist' = M.insertWith (++) curr [(fst r, h)] hist
        s' = newRock s $ snd r

extrapolate hist cycles =
  -- billion = a + n * xmod + dx
  -- answer  = b + n * ymod + dy
  let billion = 1000000000000
      [[_, (c,d), (a,b)]] = M.elems cycles
      [xmod, ymod] = [(c-a), (d-b)]
      dx = (billion `mod` xmod) + (xmod - a)
      dy = let f = ((M.fromList . concat $ M.elems hist) M.!)
           in f (a+dx) - f a
      n = div (billion - dx - a) xmod
  in b + n * ymod + dy

solve2 contents =
  let floor = S.fromList . zip [0..6] $ repeat 0
      jets = concat . repeat . zip [0..] $ contents
      rocks = zip [0..] . concat . repeat $ [r1,r2,r3,r4,r5]
  in findRepeats (floor, 0, jets) M.empty rocks

-- Main and debugging

visualise rocks =
  let [lo, hi] = map ($ S.map snd rocks) [minimum, maximum]
      coords = [[(x,y) | x <- [0..6]] | y <- [hi, hi-1 .. lo]]
      grid = map (map (\x -> if x `S.member` rocks then '#' else '.')) coords
  in unlines grid

main = do
  contents <- getLine
  print $ solve1 contents
  print $ solve2 contents
