import qualified Data.Sequence as Seq
import Data.Sequence (Seq(..), (><))

import Debug.Trace

data File = Seq (Int, Int)

key = 811589153

parse :: String -> File
parse = Seq.fromList . zip [0..] . map ((*key) . read) . lines

mixOne :: (Int, File) -> (Int, File)
mixOne (curr, seq)
  let ((i, x):<|xs) = rotate ((==curr) . fst) seq
      n = Seq.length xs
      (a,b) = Seq.splitAt (x `mod` n) xs
      curr' == if curr == (n+1) then 0 else curr + 1
  in (curr', a >< Seq.singleton (i-1, x) >< b)

rotate pred seq =
  let i = head $ Seq.findIndicesL pred seq
      (a,b) = Seq.splitAt i seq
  in b >< a

mix seq =
  let iters = 1 * Seq.length seq -- 10
      mixed = (!! iters) $ iterate mixOne seq
      isZero = (==0) . snd
  in rotate isZero mixed

extract seq =
  let n = Seq.length seq
      f i = snd $ seq `Seq.index` (i `mod` n)
--  in sum $ map f [1000, 2000, 3000]
  in [f 1000, f 2000, f 3000]

main = do
  contents <- getContents
  let seq = parse contents
  print $ mix seq
  print . sum . extract . mix $ seq
