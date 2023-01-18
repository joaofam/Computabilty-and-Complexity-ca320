evalPoly :: Int -> [Int] -> Int
evalPoly x [] = 0 -- if empty return 0
evalPoly x (value:values) = value + x * (evalPoly x values)