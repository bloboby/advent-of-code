import Data.Char (isDigit, isSpace)
import Data.List (foldl')
import Data.Maybe (catMaybes)

import Debug.Trace

data Blueprint = Blueprint { idno :: Int
                           , oo :: Int
                           , co :: Int
                           , bo :: Int
                           , bc :: Int
                           , go :: Int
                           , gb :: Int
                           , oMax :: Int
                           } deriving (Show)

data Resources = Resources { t :: Int
                           , o :: Int
                           , c :: Int
                           , b :: Int
                           , g :: Int
                           , oBot :: Int
                           , cBot :: Int
                           , bBot :: Int
                           , gBot :: Int
                           } deriving (Show)

-- Resource helpers

makeOBot bp res
  | oBot res >= oMax bp = Nothing
  | o res >= oo bp      = Just res { o = o res - oo bp, oBot = oBot res + 1 }
  | otherwise           = Nothing

makeCBot bp res
  | cBot res >= bc bp = Nothing
  | o res >= co bp    = Just res { o = o res - co bp, cBot = cBot res + 1 }
  | otherwise         = Nothing

makeBBot bp res
  | bBot res >= gb bp = Nothing
  | o res >= bo bp && c res >= bc bp
      = Just res { o = o res - bo bp, c = c res - bc bp, bBot = bBot res + 1 }
  | otherwise = Nothing

makeGBot bp res
  | o res >= go bp && b res >= gb bp
      = Just res { o = o res - go bp, b = b res - gb bp, gBot = gBot res + 1 }
  | otherwise = Nothing

produce res = res { t = t res + 1
                  , o = o res + oBot res
                  , c = c res + cBot res
                  , b = b res + bBot res
                  , g = g res + gBot res }

-- Main

parse :: String -> Blueprint
parse s =
  let parseLine = map read . words . filter (\x -> isDigit x || isSpace x)
      [a,b,c,d,e,f,g] = parseLine s
  in Blueprint a b c d e f g $ maximum [b,c,d,f]

getNbrs bp res =
  -- Can prune the |Just res| case further.
  map produce $ case makeGBot bp res of
    Just res' -> [res]
    -- TODO: need to produce before adding to the bot count
    Nothing   -> catMaybes [makeOBot bp res, makeCBot bp res, makeGBot bp res, Just res]

maxGeodes bp =
  let oneMinute = trace "minute" $ foldl' (\acc r -> (getNbrs bp r) ++ acc) []
      end = (!!8) $ iterate oneMinute [Resources 0 0 0 0 0 1 0 0 0]
  in end -- maximum $ map g end

-- solve :: [Blueprint] -> Int
solve = map maxGeodes -- maximum . map (\bp -> idno bp * maxGeodes bp)

main = do
  contents <- getContents
  let blueprints = map parse $ lines contents
  putStrLn . unlines . map show $ maxGeodes $ head blueprints
