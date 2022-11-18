
is_empty :: [Int] -> Bool
is_empty [] = True
is_empty (x : xs) = False

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = (fib (n - 1)) + (fib (n - 2))

elength :: [Int] -> Int
elength [] = 0
elength (x:xs) = 1 + (elength xs)

eeven :: [Int] -> Bool
eeven [] = True
eeven (x:[]) = False
eeven (x:xs:xt) = if (elength xt == 0)
    then True
    else if (elength xt == 1)
        then False
    else eeven xt

ereverse :: [Int] -> [Int]
ereverse [] = []
ereverse (x:xs) = (reverse xs) ++ [x]

eweave :: [Int] -> [Int] -> [Int]
eweave [] [] = []
eweave x [] = x
eweave [] y = y
eweave (x:xs) (y:ys) = ([x] ++ [y]) ++ (eweave (xs) (ys))

main = do

    print $ is_empty ([1])

    print $ fib 6

    print $ elength ([1,2,3])

    print $ eeven ([1,2,3,4])

    print $ ereverse ([1,2,3])

    print $ eweave [0,2,4] [1,3,5]