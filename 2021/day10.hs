import qualified Data.Map as M
import Data.List (sort)
import Data.Maybe (mapMaybe)
import Util
 
scoreChar c = M.findWithDefault 0 c . M.fromList $ zip "([{<" [1..]
getPair c = (M.fromList $ zip ")]}>" "([{<") M.! c

scoreLine :: [Char] -> [Char] -> Maybe Int
scoreLine s [] = Just $ foldl addScore 0 s
  where addScore score c = score * 5 + scoreChar c
scoreLine s (x:xs)
  | scoreChar x > 0 = scoreLine (x:s) xs
  | length s > 0 && head s == getPair x = scoreLine (tail s) xs
  | otherwise = Nothing

solve = median . sort . mapMaybe (scoreLine [])
  where median l = (sort l) !! (length l `div` 2)

main = interact $ Util.print . solve . lines
