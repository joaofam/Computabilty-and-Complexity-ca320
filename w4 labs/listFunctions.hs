myAppend :: [a] -> [a] -> [a]
myAppend [] xs = xs
myAppend (y:ys) xs  = y:(myAppend ys xs)

myHead :: [a] -> a
myHead [] = error "Empty"
myHead (x:xs) = x

myLast :: [a] -> a
myLast [] = error "Empty"
myLast [x] = x
myLast (x:xs) = myLast xs

myTail :: [a] -> [a]
myTail [] = error "Empty"
myTail (x:xs) = xs

myInit :: [a] -> [a]
myInit [] = error "Empty"
myInit [a] = []
myInit (x:xs) = x:(myInit xs)

myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (x:xs) = x ++ (myConcat xs) -- ++ is list concentrate operator

mySum :: Num a => [a] -> a
mySum [] = 0
mySum (x:xs) = x + (mySum xs)

myProduct :: Num a => [a] -> a
myProduct [] = 0
myProduct (x:xs) = x * (myProduct xs)

myMaximum :: Ord a => [a] -> a
myMaximum [] = error "Empty"
myMaximum [x] = x
myMaximum (x:xs) = if x > (myMaximum xs)
                    then x
                    else myMaximum xs

myMinimum :: Ord a => [a] -> a
myMinimum [] = error "Empty"
myMinimum [x] = x
myMinimum (x:xs) = if x < (myMinimum xs)
                    then x
                    else myMinimum xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myelem y (x:xs) = if y == x
                    then True
                    else myElem y xs

myDelete :: Eq a => [a] -> [a] -> [a]
myDelete [] _ = []
myDelete y (x:xs) = if y == x
                    then xs
                    else x:(myDelete y xs)
