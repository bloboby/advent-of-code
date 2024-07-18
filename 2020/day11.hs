import Data.Maybe

-- hardcoded because i cbf passing constants everywhere
(m,n) = (89,90)
indices = [[(x,y) | y <- [0..n]] | x <- [0..m]]


getNbr :: [String] -> (Int, Int) -> (Int, Int) -> Maybe Char
getNbr grid (x,y) (dx,dy)
    | x' < 0 || x' > m = Nothing
    | y' < 0 || y' > n = Nothing
    | otherwise = let seat = (grid!!x')!!y' in
        if seat == '.' then getNbr grid (x',y') (dx,dy)
        else Just seat
  where
    (x',y') = (x+dx, y+dy)

updateSeat :: [String] -> (Int, Int) -> Char
updateSeat grid (x,y)
    | seat == 'L' && numNbrs == 0 = '#'
    | seat == '#' && numNbrs >= 5 = 'L'
    | otherwise = seat
  where
    seat = (grid!!x)!!y
    numNbrs = length.filter (=='#').mapMaybe (getNbr grid (x,y)) $ ds
    ds = [(-1,-1), (-1,0), (-1,1), (0,-1), (0,1), (1,-1), (1,0), (1,1)]

countFinal :: [String] -> Int
countFinal grid
    | grid == grid' = length.filter (=='#').concat $ grid
    | otherwise = countFinal grid'
  where
    grid' = map (map (updateSeat grid)) indices


main = do
    contents <- getContents
    let grid = filter (not.null) $ lines contents
    putStrLn.show $ countFinal grid

