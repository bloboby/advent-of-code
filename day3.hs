
countTrees :: (Int, Int) -> [String] -> Int
countTrees (yStep, xStep) grid =
    sum [fromEnum $ (grid!!x)!!y == '#'| (x,y) <- path]
  where
    path = zip [0, xStep..n-1] $ map (`mod` m) [0, yStep..]
    (n,m) = (length grid, length $ grid!!0)

solve :: [String] -> Int
solve grid = product [countTrees s grid | s <- steps]
  where steps = [(1,1), (3,1), (5,1), (7,1), (1,2)]

main = interact $ show.solve.lines

