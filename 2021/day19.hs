import qualified Data.Set as S
import Control.Applicative
import Data.Either (partitionEithers)
import Data.Either.Combinators (maybeToRight)
import Data.List (transpose)
import Data.List.Split (splitOn)
import Util

type Report = [[Int]]
type Scanner = ([Int], Report)

parse :: String -> [Report]
parse s = map (map (map read . splitOn ",")) scanners
  where scanners = map (tail . lines) $ splitOn "\n\n" s

-- check for overlaps

rotate :: [Int] -> [[Int]]
rotate [x,y,z] =
  [[x,y,z], [x,-y,-z], [-x,y,-z], [-x,-y,z],
   [z,x,y], [z,-x,-y], [-z,x,-y], [-z,-x,y],
   [y,z,x], [y,-z,-x], [-y,z,-x], [-y,-z,x],
   [-x,z,y], [x,-z,y], [x,z,-y], [-x,-z,-y],
   [-z,y,x], [z,-y,x], [z,y,-x], [-z,-y,-x],
   [-y,x,z], [y,-x,z], [y,x,-z], [-y,-x,-z]]

tryOverlap :: Report -> Report -> Either Report Scanner
tryOverlap s1 s2 = maybeToRight s2 s2'
  where rotations = transpose $ map rotate s2
        s2' = foldl1 (<|>) $ map (tryTranslate s1) rotations

tryTranslate :: Report -> Report -> Maybe Scanner
tryTranslate s1 s2 = if null s2s then Nothing else Just $ head s2s
  where s1' = S.fromList s1
        valid = (>=12) . length . S.intersection s1' . S.fromList
        diffs = [zipWith (-) b1 b2 | b1 <- s1, b2 <- s2]
        translated = [ map (zipWith (+) d) s2  | d <- diffs]
        s2s = filter (valid . snd) $ zip diffs translated

-- assemble the map

process :: [Report] -> [Report] -> [[Int]] -> [[Int]]
process _ [] scanners = scanners
process (l:located) unknown scanners =
  let (unknown', l') = partitionEithers $ map (tryOverlap l) unknown
      located' = located ++ (map snd l')
      scanners' = scanners ++ (map fst l')
  in process located' unknown' scanners'

solve scanners = maximum dists
  where coords = process [head scanners] (tail scanners) [[0,0,0]]
        pairs = [(x,y) | x <- coords, y <- coords, x /= y]
        dists = map (sum . map abs . uncurry (zipWith (-))) pairs

main = interact $ Util.print . solve . parse
