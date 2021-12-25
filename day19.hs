import qualified Data.Map as M
import Data.Char (isAlpha)
import Data.List.Split (splitOn)
import Text.ParserCombinators.ReadP
import Util

data Rule = Literal Char | Seq [Int] | Oneof [Rule] deriving Show
type Rules = M.Map Int Rule

-- preprocess

parseRule :: String -> (Int, Rule)
parseRule s
  | any isAlpha $ head rest =
      let [_,c,_] = head rest
      in (num, Literal c)
  | all (/="|") rest = (num, Seq $ map read rest)
  | otherwise =
      let l = takeWhile (/="|") rest
          r = tail $ dropWhile (/="|") rest
      in (num, Oneof [Seq $ map read l, Seq $ map read r])
  where (n:rest) = words s
        num = read $ init n

preprocess :: String -> (Rules, [String])
preprocess s = (rules, lines msgs)
  where [r, msgs] = splitOn "\n\n" s
        rules = M.fromList . map parseRule $ lines r

modify :: (Rules, [String]) -> (Rules, [String])
modify (rules, msgs) = (rules', msgs)
  where r8 = Oneof [Seq [42], Seq [42, 8]]
        r11 = Oneof [Seq [42, 31], Seq [42, 11, 31]]
        rules' = M.insert 8 r8 . M.insert 11 r11 $ rules

-- make parsers

countValid :: (Rules, [String]) -> Int
countValid (rules, msgs) =
  let parse = readP_to_S (makeParser $ rules M.! 0)
      valid = any (null . snd)
  in length . filter valid . map parse $ msgs
  where
    makeParser :: Rule -> ReadP Char
    makeParser rule =
      case rule of
        Literal c -> char c
        Seq ns -> makeSeqParser ns
        Oneof rs -> choice (map makeParser rs)

    makeSeqParser :: [Int] -> ReadP Char
    makeSeqParser (n:ns) =
      let p = makeParser $ rules M.! n
      in if null ns then p else p >> makeSeqParser ns

main = interact $ Util.print . countValid . modify . preprocess
