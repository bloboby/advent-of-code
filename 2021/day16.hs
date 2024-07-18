import Data.Either (fromRight)
import Text.Parsec.String (Parser)
import Text.ParserCombinators.Parsec
import Util

data Packet = Literal Int Int Int | Operator Int Int [Packet]
              deriving Show

hexToBin :: String -> String
hexToBin = concatMap f where
  f c = case c of
    '0' -> "0000"
    '1' -> "0001"
    '2' -> "0010"
    '3' -> "0011"
    '4' -> "0100"
    '5' -> "0101"
    '6' -> "0110"
    '7' -> "0111"
    '8' -> "1000"
    '9' -> "1001"
    'A' -> "1010"
    'B' -> "1011"
    'C' -> "1100"
    'D' -> "1101"
    'E' -> "1110"
    'F' -> "1111"

binToDec :: String -> Int
binToDec = foldl (\acc c -> if c=='0' then acc*2 else acc*2+1) 0

-- parsers

decode :: Parser Packet
decode = do
  v <- binToDec <$> count 3 digit
  t <- binToDec <$> count 3 digit
  if t == 4 then do
    literal <- binToDec <$> decodeLiteral
    return $ Literal v t literal
  else do
    packets <- decodeOperator
    return $ Operator v t packets

decodeLiteral :: Parser String
decodeLiteral = do
  prefix <- digit
  group <- count 4 digit
  if prefix == '1' then do
    more <- decodeLiteral
    return $ group ++ more
  else return group

decodeOperator :: Parser [Packet]
decodeOperator = do
  ltype <- digit
  if ltype == '0' then do
    length <- binToDec <$> count 15 digit
    more <- count length digit
    let packets = fromRight [] $ parse (many1 decode) "" more
    return packets
  else do
    subs <- binToDec <$> count 11 digit
    packets <- count subs decode
    return packets

-- extract

sumVersions :: Packet -> Int
sumVersions (Literal v _ _) = v
sumVersions (Operator v _ pkts) = v + sum (map sumVersions pkts)

calculate :: Packet -> Int
calculate (Literal _ _ n) = n
calculate (Operator _ t packets) =
  if t<5 then
    let f = [sum, product, minimum, maximum] !! t
    in f $ map calculate packets
  else
    let f = [(>), (<), (==)] !! (t-5)
        [a,b] = map calculate packets
    in fromEnum $ f a b

solve s = map f $ lines s
  where p = fromRight undefined . parse decode ""
        f = calculate . p . hexToBin

main = interact $ Util.print . solve
