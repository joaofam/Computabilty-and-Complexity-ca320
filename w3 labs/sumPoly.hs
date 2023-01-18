{-
> sumPoly [1,7,5,2] [42,2,1]
[43,9,6,2]

> sumPoly [-3,0,0,0,1] [1,7,5,2]
[-2,7,5,2,1]

> sumPoly [0,-2,0,4] [1,7,5,2]
[1,5,5,6]
-}

type Poly = [Int]

sumPoly :: Poly -> Poly -> Poly
sumPoly [] p = p
sumPoly p [] = p
sumPoly (x:xs) (y:ys) = (x + y):(sumPoly xs ys)
