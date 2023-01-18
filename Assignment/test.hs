module Data.Numbers.Primes ( primes, wheelSieve ) where

sieve :: [Int] -> [Int]
sieve []     = []
sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p /= 0]

wheel = 2:4:2:4:6:2:6:4:2:4:6:6:2:6:4:2:6:4:6:8:4:2:4:2:4:8:6:4:6:2:4:6:2:6:6:4:2:4:6:2:6:4:2:4:2:10:2:10:wheel

primes :: Integral int => [int]
primes = wheelSieve 6

wheelSieve :: Integral int
           => Int    -- ^ number of primes canceled by the wheel
           -> [int]  -- ^ infinite list of primes
wheelSieve k = reverse ps ++ map head (sieve p (cycle ns))
 where (p:ps,ns) = wheel k

isPrime :: Integral int => int -> Bool
isPrime n | n > 1     = primeFactors n == [n]
          | otherwise = False

primeFactors :: Integral int => int -> [int]
primeFactors n = factors n (wheelSieve 6)
 where
  factors 1 _                  = []
  factors m (p:ps) | m < p*p   = [m]
                   | r == 0    = p : factors q (p:ps)
                   | otherwise = factors m ps
   where (q,r) = quotRem m p

firstPerfect :: Int -> [Integer]
firstPerfect n = take n [2 ^ (x - 1) * (2 ^ x - 1) | x <- primes, isPrime(2 ^ x - 1)]

perfectNumbersUpTo :: Integer -> [Integer]
perfectNumbersUpTo n = 
    takeWhile (< n) [2 ^ (x - 1) * (2 ^ x - 1) | x <- primes, isPrime(2 ^ x - 1)]

main = print $ perfectNumbersUpTo 1000