isDivisor :: Integral a => a -> a -> Bool
isDivisor n a = rem n a == 0

sumDivisors :: Integral a => a -> a
sumDivisors a =
   foldr (\n -> if isDivisor a n then (+ n) else id) 0 [1..(a `div` 2)]

isPerfect :: Integral a => a -> Bool
isPerfect n = n == sumDivisors n

-- The main prime-finding function:
upUntil :: Integral a => a -> [a]
upUntil n = filter isPerfect [1..n]

main = print $ upUntil (10000 :: Int)