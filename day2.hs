import Data.List.Split

parse :: String -> (Int, Int, Char, String)
parse s = let [a,b,[c],d] = wordsBy (`elem` "-: ") s
    in (read a, read b, c, d)

valid :: (Int, Int, Char, String) -> Bool
valid (a,b,c,d) = let l = length $ filter (==c) d
    in l >= a && l <= b

valid' :: (Int, Int, Char, String) -> Bool
valid' (a,b,c,d) = (d!!(a-1) == c) /= (d!!(b-1) == c)

main = interact $ show
    .length.filter (==True)
    .map valid'
    .map parse.lines

