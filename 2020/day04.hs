import Data.Char
import Data.List.Split
import qualified Data.Map as M
import qualified Data.Set as S

inRange :: Int -> Int -> String -> Bool
inRange start end s = all isDigit s && start <= n && n <= end
  where n = read s

validHeight :: String -> Bool
validHeight s | units == "cm" = inRange 150 193 h
              | units == "in" = inRange 59 76 h
              | otherwise = False
  where (h, units) = break isAlpha s

valid :: String -> Bool
valid s = S.fromList fields `S.isSubsetOf` (M.keysSet f)
    && inRange 1920 2002 (f M.! "byr")
    && inRange 2010 2020 (f M.! "iyr")
    && inRange 2020 2030 (f M.! "eyr")
    && validHeight (f M.! "hgt")
    && let hcl = f M.! "hcl"
       in (length hcl == 7 && head hcl == '#'
           && all (`elem` "0123456789abcdef") (tail hcl))
    && (f M.! "ecl") `elem` ["amb","blu","brn","gry","grn","hzl","oth"]
    && let pid = f M.! "pid"
       in (all isDigit pid && length pid == 9)
  where
    fields = ["byr","iyr","eyr","hgt","hcl","ecl","pid"]
    f = M.fromList.map makePair.words $ s
    makePair w = let [k,v] = splitOn ":" w in (k,v)

main = interact $ show
    .length.filter (==True).map valid
    .splitOn "\n\n"

