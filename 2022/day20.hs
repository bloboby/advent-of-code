import qualified Data.Sequence as Seq
import Data.Sequence (Seq(..), (><))

type File = Seq (Int, Int)

key = 811589153

parse :: String -> File
parse = Seq.fromList . zip [0..] . map ((*key) . read) . lines

mixOne :: (Int, File) -> (Int, File)
mixOne (curr, seq) =
  let ((i, x):<|xs) = rotate ((==curr) . fst) seq
      n = Seq.length xs
      (a,b) = Seq.splitAt (x `mod` n) xs
      curr' = if curr == n then 0 else curr + 1
  in (curr', a >< Seq.singleton (i, x) >< b)

rotate pred seq =
  let i = head $ Seq.findIndicesL pred seq
      (a,b) = Seq.splitAt i seq
  in b >< a

mix seq =
  let iters = 10 * Seq.length seq
      mixed = snd . (!! iters) . iterate mixOne $ (0, seq)
      isZero = (==0) . snd
  in rotate isZero mixed

extract seq =
  let n = Seq.length seq
      f i = snd $ seq `Seq.index` (i `mod` n)
  in sum $ map f [1000, 2000, 3000]

main = do
  contents <- getContents
  let ans = extract . mix . parse $ contents
  print ans
