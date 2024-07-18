import Data.Char
import Data.List.Split
import qualified Data.IntMap as M


decToBin :: Int -> [Int] -> [Int]
decToBin 0 v2 = v2
decToBin v v2 = decToBin (v `div` 2) $ (v `mod` 2):v2

revBinToDec :: [Int] -> Int
revBinToDec b = sum $ zipWith (*) b $ iterate (*2) 1

-- part 1

maskVal :: String -> Int -> Int
maskVal mask v = revBinToDec $ map combine grouped
  where
    combine (mask, bit) = if mask == 'X' then bit else digitToInt mask
    grouped = zip (reverse mask) (reverse v2 ++ repeat 0)
    v2 = decToBin v []

-- part 2

updateMem :: M.IntMap Int -> String -> Int -> Int -> M.IntMap Int
updateMem mem mask k v = foldr (flip M.insert v) mem ks
  where
    ks = map revBinToDec $ foldr f [[]] $ map combine grouped
    f x acc = if x == 'X' then (map (0:) acc) ++ (map (1:) acc)
        else map (digitToInt x:) acc
    combine (mask, bit) = if mask == '0' then intToDigit bit else mask
    grouped = zip (reverse mask) (reverse v2 ++ repeat 0)
    v2 = decToBin k []

-- common

writeAll :: M.IntMap Int -> String -> [String] -> Int
writeAll mem _ [] = sum.M.elems $ mem
writeAll mem mask (x:xs)
    | lhs == "mask" = writeAll mem rhs xs
    | otherwise =
        let k = read $ filter isDigit lhs
            v = read rhs
            -- mem' = M.insert k (maskVal mask v) mem
            mem' = updateMem mem mask k v
        in writeAll mem' mask xs
  where
    [lhs, rhs] = splitOn " = " x

main = interact $ show.writeAll M.empty "".filter (not.null).lines

