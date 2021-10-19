import qualified Data.IntMap as M

input :: [Int]
input = [16,1,0,18,12,14,19]

play :: Int -> Int -> M.IntMap (Int, Int) -> Int
play turn prevNum history
    | turn == 30000000 = num
    | otherwise = play (turn+1) num $ M.insert num (turn, age) history
  where
    num = snd $ history M.! prevNum
    age = case M.lookup num history of
        Just (prevTurn, _) -> turn - prevTurn
        Nothing -> 0

main = putStrLn.show
    .play (length input+1) (last input)
    .M.fromList $ zip input $ zip [1..] $ repeat 0

