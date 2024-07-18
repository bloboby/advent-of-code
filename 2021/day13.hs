import qualified Data.Set as S
import Data.List.Split (splitOn)

type Dots = S.Set [Int]
type Folds = [(String, Int)]

parse :: String -> (Dots, Folds)
parse s = (dots, folds)
  where [d, f] = splitOn "\n\n" s
        parseFold = (\[ax,n] -> (ax, read n)) . splitOn ("=")
        dots = S.fromList . map (map read . splitOn ",") $ lines d
        folds = map (parseFold . last . words) $ lines f

makeFolds :: (Dots, Folds) -> Dots
makeFolds (dots, folds) = foldr makeFold dots $ reverse folds
  where makeFold fold = S.map (mirror fold)
        mirror (ax,n) [x,y] = let f m z = if z <= m then z else 2*m-z
          in if ax == "x" then [f n x, y] else [x, f n y]

display :: Dots -> String
display dots = unlines $ [[getChar [x,y] | x <- [0..xmax]]
                                         | y <- [0..ymax]]
  where getChar pt = if pt `S.member` dots then '#' else '.'
        [xmax, ymax] = map S.findMax [S.map head dots, S.map last dots]

main = interact $ display . makeFolds . parse
