import Data.Either (fromRight)
import Data.List (elemIndex, sort)
import Data.List.Split (chunksOf)
import Data.Maybe (mapMaybe)
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec

data Value = List [Value] | Integer Int deriving (Eq, Show)
instance Ord Value where compare = compareValues

-- parse

parseValue :: Parser Value
parseValue = try parseList <|> parseInteger

parseList = do
  char '['
  contents <- parseValue `sepBy` (char ',')
  char ']'
  return $ List contents

parseInteger = do
  n <- many1 digit
  notFollowedBy digit
  return $ Integer (read n)

-- compare

compareValues (Integer l) (Integer r) = compare l r
compareValues l@(Integer _) (List r) = compareValues (List [l]) (List r)
compareValues (List l) r@(Integer _) = compareValues (List l) (List [r])
compareValues (List l) (List r)
  | null l && null r = EQ
  | null l && not (null r) = LT
  | not (null l) && null r = GT
  | cmp /= EQ = cmp
  | otherwise = compareValues (List as) (List bs)
  where (a:as) = l
        (b:bs) = r
        cmp = compareValues a b

-- solve

parseLine = fromRight undefined . parse parseValue ""

solve1 packets =
  let filterLT (index, [l,r]) = if l < r then Just index else Nothing
  in sum . mapMaybe filterLT . zip [1..] . chunksOf 2 $ packets

solve2 packets =
  let d@[d1, d2] = map parseLine ["[[2]]", "[[6]]"]
      allPackets = sort (d1:d2:packets)
  in product . map (+1) $ mapMaybe (`elemIndex` allPackets) d

main = do
  contents <- getContents
  let packets = map parseLine . filter (not . null) $ lines contents
  print (solve1 packets, solve2 packets)
