{- 
Sum of all perfect numbers between 1 to 1,000,000,000,000 (1 trillion)
Perfect number is a number that is equal to the sum of its factors
e.g 6 -> 1 + 2 + 3 = 6
28 -> 1 + 2 + 4+ 14 = 28
use high order functions

List of Perfect numbers = 6, 28, 496, 8128, 335503336, 8589869056, 137438691328
sum = 146062119378

perfect number = factors (numbers that return 0 when modulus)
formula = 2^p−1(2^p − 1)

-}

-- Formula to calculate perfect numbers
perfectFormula :: Int -> Int
perfectFormula p = 2^(p-1) * (2^p -1)

-- Sum of Divisors formula with the aid of https://solveforum.com/forums/threads/solved-finding-perfect-numbers-in-haskell.287739/#post-287751
sumDivisors :: Integral a => a -> a
sumDivisors a = foldr (\n -> let (q,r) = a `quotRem` n in -- foldr combines list entries with values that are continuosly rising. quotRem
       if r==0
       then (+ (n+q)) else id) 1
       [2..(floor . sqrt . fromIntegral $ a)] -- floor is to round values down. fromIntegral converts an int calculation wih a float to a float.

-- check if a number is perfect from the perfect formula using divisor function
isPerfect :: Integral a => a -> Bool
isPerfect n = n == sumDivisors n

-- valid function to make sure numbers don't go past 1 trillion
valid :: Int -> Bool
valid x
    | x < 1000000000000 && x > 1 = True -- if they're under 1 trillion then true
    | otherwise = False -- else it is false

-- function to filter through all the values and calculate the perfect numbers
valueReach :: Int -> [Int]
valueReach n = filter valid (fmap perfectFormula [1..n]) -- filter with valid and fmap (functor over perfectFormula from 1 to n value )

-- main function to print the sum of the perfect numbers
main :: IO ()
main = print $ sum $ filter isPerfect $ valueReach 19

-- High Order - https://www.some.ox.ac.uk/wp-content/uploads/2019/08/HaskellTutorial.pdf
-- https://codereview.stackexchange.com/questions/86689/brute-force-perfect-number-algorithm
-- https://solveforum.com/forums/threads/solved-finding-perfect-numbers-in-haskell.287739/#post-287751
-- https://hackage.haskell.org/package/arithmoi-0.1.0.1/docs/src/Math-NumberTheory-Primes-Sieve-Misc.html#FactorSieve
-- https://hackage.haskell.org/package/arithmoi-0.1.0.1/docs/src/Math-NumberTheory-Primes-Factorisation-Montgomery.html#factorise
-- https://eprints.whiterose.ac.uk/3784/1/runcimanc1.pdf