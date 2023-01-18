
-- 1c
reverse [] = []
reverse (x:xs) = reverse xs ++ [x]

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = error"Empty list"
isPalindrome x = if reverse x == x
                    then True
                    else False

-- 2b
evalPoly :: Int -> [Int] -> Int
evalPoly x [] = 0 -- if empty return 0
evalPoly x (value:values) = value + x * (evalPoly x values)

-- 2c test case = [[1,2], [1], [1,4,6]]
shortest :: [[a]] -> [a]
shortest [] = []
shortest [x] = x
shortest (first:second) = if leAngth first > length (shortest second)
                    then shortest second
                    else first
