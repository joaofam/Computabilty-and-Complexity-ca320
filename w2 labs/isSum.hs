{-
Design a Haskell function isSum that takes three integer arguments and tests whether
one of them is the sum of the other two.
The behaviour of isSum should be as follows:
> isSum 1 2 3
True
> isSum 4 9 5
True
> isSum 12 5 7
True
> isSum 23 23 23
False
You should start by declaring the type of isSum in your script
-}

isSum :: Int -> Int -> Int -> Bool
isSum a b c =
     (a + b == c) 
     || (b + c == a)
     || (c + a == b)