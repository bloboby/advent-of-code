import Data.Char

-- part 1

type Op = (Int, Char)

eval :: [Op] -> Op -> String -> Int
eval _ (n,_) [] = n
eval stack op@(n,_) (c:cs)
    | c == '(' = eval (op:stack) (0,'+') cs
    | c == ')' = eval (tail stack) (f (head stack) n, '\0') cs
    | isDigit c = eval stack (f op $ digitToInt c, '\0') cs
    | otherwise = eval stack (n,c) cs
  where
    f (x, op) y = if op == '+' then x+y else x*y

evaluate :: String -> Int
evaluate = eval [] (0,'+').filter (/=' ')

-- part 2

type Coeff = (Int, Int)

eval' :: [Coeff] -> Coeff -> String -> Int
eval' _ (a,b) [] = a*b
eval' stack ab@(a,b) (c:cs)
    | c == '(' = eval' (ab:stack) (1,0) cs
    | c == ')' = let (a',b') = head stack in
        eval' (tail stack) (a', b'+a*b) cs
    | isDigit c = eval' stack (a, b+digitToInt c) cs
    | c == '*' = eval' stack (a*b, 0) cs
    | c == '+' = eval' stack ab cs

evaluate' :: String -> Int
evaluate' = eval' [] (1,0).filter (/=' ')


main = interact $ show.sum.map evaluate'.filter (not.null).lines

