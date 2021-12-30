import qualified Data.Map as M
import Data.Char (digitToInt)
import Data.List (elemIndex, sort)
import Data.Maybe (fromJust)
import Util

type IM = M.Map Int Int

initialise :: [Int] -> ([IM], Int)
initialise v = (arrs, idx1)
  where
    n = 10^6
    vals = v ++ [length v + 1 .. n]
    pred = let idx = M.fromList $ zip v [0..]
               f (val, i) = if val==1 then (i, n-1)
                            else (i, idx M.! (val-1))
               a = map snd . sort . map f $ M.toList idx
               b = (idx M.! length v):[length v .. n-2]
           in a++b
    next = [1 .. n-1] ++ [0]
    arrs = map (M.fromList . zip [0..]) [vals, pred, next]
    idx1 = fromJust $ elemIndex 1 v

perform :: IM -> IM -> (Int, IM) -> (Int, IM)
perform vals pred (current, next) = (next'' M.! current, next'')
  where
    -- Remove the next three cups.
    first = next M.! current
    second = next M.! first
    third = next M.! second
    next' = M.insert current (next M.! third) next

    -- Find the destination cup.
    getDest i xs = if (vals M.! i) `notElem` xs then i
                   else getDest (pred M.! i) xs
    dest = let vs = map (vals  M.! ) [first, second, third]
           in getDest (pred M.! current) vs

    -- Insert the removed cups.
    next'' = M.insert third (next' M.! dest)
           . M.insert dest first $ next'

solve ([vals, pred, next], idx1) = cup1 * cup2
  where moves = iterate (perform vals pred) (0, next)
        next' = snd (moves !! (10^7))
        cup1 = vals M.! (next' M.! idx1)
        cup2 = vals M.! (next' M.! (next' M.! idx1))

main = interact $ Util.print . solve . initialise . parse
  where parse = map digitToInt . head . lines
