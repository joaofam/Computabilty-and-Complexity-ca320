{-
Have you considered what your triangleArea function from exercise 1 will do with
invalid data? For example, there is no triangle with sides 1, 2, 4. What does GHCi give as
the value of the expression: triangleArea 1 2 4?
-}

valid :: Float -> Float -> Float -> Bool
valid  a b c 
    | (a + b < c) && (a + c > b) && (b + c > a) = False
    | otherwise = True

triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c 
    | valid a b c == False = error "Not a triangle"
    | otherwise = sqrt(s*(s-a)*(s-b)*(s-c))
    where s = (a + b + c)/2 