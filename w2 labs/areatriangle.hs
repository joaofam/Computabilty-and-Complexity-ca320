-- The area of a triangle with sides a, b, c is given by the formula:
-- 
-- √s(s - a)(s - b)(s - c)
-- where
-- s = (a + b + c)/2

triangleArea :: Float -> Float -> Float -> Float
triangleArea a b c = sqrt(s*(s-a)*(s-b)*(s-c))
    where s = (a + b + c)/2 