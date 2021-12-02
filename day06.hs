import Data.List.Split
import qualified Data.Set as S

count :: String -> Int
count = length.S.fromList.filter (/='\n')

count' :: String -> Int
count' = length.foldr1 S.intersection.map S.fromList.words

main = interact $ show.sum.map count'.splitOn "\n\n"

