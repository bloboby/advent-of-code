import qualified Data.Map as M
import Util

parse :: String -> [Int]
parse = map (read . last . words) . lines

add x y = let z = (x+y) `mod` 10 in if z==0 then 10 else z

-- part 1

play [die, numRolls, p1, score1, p2, score2] =
  if score2 >= 1000 then numRolls * score1
  else let rolls = tail . take 4 $ iterate ((+1) . (`mod` 100)) die
           [rollSum, die'] = map ($ rolls) [sum, last]
           p1' = add p1 rollSum
           score1' = score1 + p1'
       in play [die', (numRolls+3), p2, score2, p1', score1']

-- part 2

type KV = ([Int], Int)

playState :: KV -> ([KV], Int)
playState ([pos, score], count) = (state', wins)
  where coeff = [(3,1), (4,3), (5,6), (6,7), (7,6), (8,3), (9,1)]
        state = [let pos' = add pos k
                 in ([pos', score+pos'], count*v) | (k, v) <- coeff]
        state' = filter (\([_,n],_) -> n<21) state
        wins = let f ([_,n],m) acc = if n>=21 then acc+m else acc
               in foldr f 0 state

play' (p1, wins1, p2, wins2) =
  if M.null p2 then max wins1 wins2
  else let states = map playState $ M.toList p1
           p1' = M.unionsWith (+) . map (M.fromList . fst) $ states
           wins1' = wins1 + (sum $ map snd states) * (foldr1 (+) p2)
       in play' (p2, wins2, p1', wins1')

solve [p1, p2] = play' (M.singleton [p1,0] 1, 0, M.singleton [p2,0] 1, 0)

main = interact $ Util.print . solve . parse
