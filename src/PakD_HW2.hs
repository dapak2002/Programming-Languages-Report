select_evens :: [a] -> [a]
select_evens [] = []
select_evens (x:xs)
    |mod (length xs) 2 == 1 = x : select_evens xs
    |otherwise = select_evens xs

select_odds :: [a] -> [a]
select_odds [] = []
select_odds (x:xs)
    |mod (length xs) 2 == 0 = x : select_odds xs
    |otherwise = select_odds xs

member :: (Eq a) => a -> [a] -> Bool
member a [] = False
member a (x:xs)
    |a == x = True
    |otherwise = member a xs

append :: [a] -> [a] -> [a]
append xs [] = xs
append xs (y:ys) = append (xs ++ [y]) ys

revert :: [a] -> [a]
revert [] = []
revert (x:xs) = (revert xs) ++ [x]

less_equal :: [Int] -> [Int] -> Bool
less_equal [] [] = True
less_equal (x:xs) (y:ys)
    |x <= y = less_equal xs ys
    |otherwise = False