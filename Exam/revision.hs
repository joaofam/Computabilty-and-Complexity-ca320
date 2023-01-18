-- 2013

evalPoly :: Poly -> Float -> Float
evalPoly [p] _ = p
evalPoly (p:ps) x = p + (x * (evalPoly ps x))

shortest :: [[a]] -> [a]
shortest [] = []
shortest [x] = x
shortest (x:y:list)
    | length x > length y = shortest (y:list)
    | otherwise = shortest (x:list)


longest :: [[a]] -> [a]
longest [x] = x
longest (x:xs) = if length x > length (longest xs)
                      then x
                  else longest xs