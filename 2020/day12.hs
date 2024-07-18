
move :: Int -> Int -> Int -> [(Char, Int)] -> Int
move _ north east [] = abs north + abs east
move dir north east ((x,n):xs)
    | x == 'N' = move dir (north+n) east xs
    | x == 'S' = move dir (north-n) east xs
    | x == 'E' = move dir north (east+n) xs
    | x == 'W' = move dir north (east-n) xs
    | x == 'L' = move ((dir-m) `mod` 4) north east xs
    | x == 'R' = move ((dir+m) `mod` 4) north east xs
    | x == 'F' = case dir of
        -- east is 0
        0 -> move dir north (east+n) xs
        1 -> move dir (north-n) east xs
        2 -> move dir north (east-n) xs
        3 -> move dir (north+n) east xs
  where
    m = n `div` 90

move' :: Int -> Int -> Int -> Int -> [(Char, Int)] -> Int
move' north east _ _ [] = abs north + abs east
move' north east wNorth wEast ((x,n):xs)
    | x == 'N' = move' north east (wNorth+n) wEast xs
    | x == 'S' = move' north east (wNorth-n) wEast xs
    | x == 'E' = move' north east wNorth (wEast+n) xs
    | x == 'W' = move' north east wNorth (wEast-n) xs
    | x == 'F' = move' (north+n*wNorth) (east+n*wEast) wNorth wEast xs
    | otherwise =
        let rotate (a,b) = if (x == 'L') then (b,-a) else (-b, a)
            m = n `div` 90
            (wn, we) = iterate rotate (wNorth, wEast) !! m
        in move' north east wn we xs


main = interact $ show
  .move' 0 0 1 10
  .map (\x -> (head x, read $ tail x))
  .lines

