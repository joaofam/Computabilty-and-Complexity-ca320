reverseIt :: [Int] -> [Int]
reverseIt [] = []
reverseIt (x:xs) = (reverseIt xs) ++ [x]

-- kyrilkhalestsky
myGCD :: Int -> Int -> Int

myGCD n 0 = n
myGCD m n = myGCD n (m `mod` n)

dectobin :: Int -> String

dectobin 0 = "0"
dectobin 1 = "1"
dectobin d 
   | d `mod` 2 == 0 = (dectobin (d `div` 2)) ++ "0"
   | otherwise      = (dectobin (d `div` 2)) ++ "1"


data Circuit = Component Float | Serial Circuit Circuit | Parallel Circuit Circuit

resistance :: Circuit -> Float

resistance (Component r)    = r
resistance (Serial c1 c2)   = resistance c1 + resistance c2
resistance (Parallel c1 c2) = 1 / (1/resistance c1 + 1/resistance c2)

-- imduffy15

data Score = Conversion | Try | Goal

convert :: Score -> Int
convert Conversion = 2
convert Try = 5
convert Goal = 3

sumScore :: [Score] -> Int
sumScore [] = 0
sumScore (x:xs) = (convert x) + sumScore(xs)

swap :: [a] -> [a]
swap [] = []
swap (x:y:rest) = y:x:(swap rest)
swap [x] = [x]

-- Lab Session 2
-- 
-- Sample answers

triangleArea :: (RealFloat a) => a -> a-> a-> a

triangleArea a b c = let s = (a+b+c)/2
                                in sqrt (s * (s-a) * (s-b) * (s-c))


isSum :: Int -> Int -> Int -> Bool

isSum x y z
     | x+y == z = True
     | x+z == y = True
     | y+z == x = True
     | otherwise = False



validTriangle :: (RealFloat a) => a -> a-> a-> Bool

validTriangle x y z
     | x+y < z = False
     | x+z < y = False
     | y+z < x = False
     | otherwise = True

triangleArea2 :: (RealFloat a) => a -> a-> a-> a

triangleArea2 a b c = if (validTriangle a b c)
                      then let s = (a+b+c)/2
                           in sqrt (s * (s-a) * (s-b) * (s-c))
                      else error "Not a triangle!"

-- Lab Session 3
--
-- Sample answers

-- Q1

isPalindrome :: (Eq a) => [a] -> Bool

isPalindrome x = if x == reverse x
                 then True
                 else False


-- Q2

shortest :: [[a]] -> [a]

shortest [x] = x

shortest (x:xs) = if length x < length (shortest xs)
                  then x
                  else shortest xs


-- Q3
      
type Poly = [Float]

addPolys :: Poly -> Poly -> Poly

addPolys [] p = p

addPolys p [] = p

addPolys (p:ps) (q:qs) = (p+q):(addPolys ps qs)


-- Q4

evalPoly :: Poly -> Float -> Float

evalPoly [p] _ = p

evalPoly (p:ps) x = p + (x * (evalPoly ps x))


-- Lab Session 4
--
-- Sample Answers


-- Q1

myAppend :: [a]->[a]->[a]

myAppend [] xs = xs

myAppend (y:ys) xs = y:(myAppend ys xs)


myHead :: [a] -> a

myHead [] = error "Called myHead with Empty List"

myHead (x:xs) = x



myLast :: [a] -> a

myLast [] = error "Called myLast with Empty List"

myLast [x] = x

myLast (x:xs) = myLast xs



myTail :: [a] -> [a]

myTail [] = error "Called myTail with Empty List"

myTail (x:xs) = xs


myInit :: [a] -> [a]

myInit [] = error "Called myInit with Empty List"

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

myConcat (x:xs) = x ++ (myConcat xs)


mySum :: (Num a) => [a] -> a

mySum [] = 0

mySum (x:xs) = x + (mySum xs)


myProduct :: (Num a) => [a] -> a

myProduct [] = 0

myProduct [x] = x

myProduct (x:xs) = x * (myProduct xs)


myMaximum :: (Ord a) => [a] -> a

myMaximum [] = error "Called myMaximum with an Empty List"

myMaximum [x] = x

myMaximum (x:xs) = if x > (myMaximum xs)
                   then x
                   else myMaximum xs


myMinimum :: (Ord a) => [a] -> a

myMinimum [] = error "Called myMinimum with an Empty List"

myMinimum [x] = x

myMinimum (x:xs) = if x < (myMinimum xs)
                   then x
                   else myMinimum xs


myElem :: (Eq a) => a -> [a] -> Bool

myElem _ [] = False

myElem y (x:xs) = if y == x
                  then True
                  else myElem y xs


myDelete :: (Eq a) => a -> [a] -> [a]

myDelete _ [] = []

myDelete y (x:xs) = if y == x
                    then xs
                    else x:(myDelete y xs)


-- Q2

myUnion :: (Eq a) => [a] -> [a] -> [a]

myUnion xs [] = xs

myUnion xs (y:ys) = if (myElem y xs) || (myElem y ys)
                    then myUnion xs ys
                    else myUnion (myAppend xs [y]) ys


myIntersect :: (Eq a) => [a] -> [a] -> [a]

myIntersect [] _ = []

myIntersect (x:xs) ys = if myElem x ys
                        then x:(myIntersect xs ys)
                        else myIntersect xs ys



data BinTree t = Empty | Root Int (BinTree t)(BinTree t)
                 deriving (Eq, Ord, Show)

myTree = Root 5 (Root 1 (Empty) (Root 3 Empty Empty))(Root 7 Empty Empty)

leaf x = Root x Empty Empty


addNode :: Int -> BinTree t -> BinTree t
addNode a Empty = leaf a  
addNode x (Root a left right)   
    | x < a  = Root a (addNode x left) right  
    | otherwise  = Root a left (addNode x right)


makeTree :: [Int] -> BinTree t
makeTree [] = Empty
makeTree [x] = leaf x
makeTree (x:xs) = addNode x (makeTree xs)


inorder :: BinTree t -> [Int]
inorder Empty = []
inorder (Root x left right) = inorder left ++ [x] ++ inorder right


mpsort :: [Int] -> [Int]
mpsort x = inorder (makeTree x)


hoAddNode :: (Int -> Int -> Bool) -> Int -> BinTree t -> BinTree t
hoAddNode _ a Empty = leaf a
hoAddNode fn x (Root a left right)
     | fn x a = Root a (hoAddNode fn x left) right
     | otherwise = Root a left (hoAddNode fn x right)


hoMakeTree :: (Int -> Int -> Bool) -> [Int] -> BinTree t
hoMakeTree _ [] = Empty
hoMakeTree fn (x:xs) = hoAddNode fn x (hoMakeTree fn xs)


hosort :: (Int -> Int -> Bool) -> [Int] -> [Int]
hosort fn x = inorder (hoMakeTree fn x)


-- AVL Trees
--
-- Lab 6

data AVLTree t = Empty | Root t (AVLTree t) (AVLTree t) Int
                 deriving (Eq, Ord, Show)


addAVL :: (Ord a) => a -> AVLTree a -> AVLTree a

addAVL x Empty = Root x Empty Empty 0

addAVL x (Root n left right bal_factor)
   | x < n = let newLeft = addAVL x left
             in
                 rebalance (Root n newLeft right ((height newLeft) - (height right)))
   | otherwise = let newRight = addAVL x right
                 in
                     rebalance (Root n left newRight ((height left) - (height newRight)))


getRoot :: AVLTree a -> a

getRoot Empty = error "getRoot from Empty"

getRoot (Root a _ _ _) = a


getLeft :: AVLTree a -> AVLTree a

getLeft Empty = error "getLeft from Empty"

getLeft (Root _ left _ _) = left


getRight :: AVLTree a -> AVLTree a

getRight Empty = error "getRight from Empty"

getRight (Root _ _ right _) = right


getBF :: AVLTree a -> Int

getBF Empty = 0

getBF (Root _ _ _ bf) = bf


height :: AVLTree a -> Int

height Empty = 0

height (Root _ left right _) = 1 + (max (height left) (height right))


rebalance :: AVLTree a -> AVLTree a

rebalance Empty = Empty

rebalance (Root r Empty Empty bf) = Root r Empty Empty bf

rebalance (Root r left right bf)
    | bf == -2 && rbf == -1 = let rl = (getLeft right)
                              in
                                  (Root (getRoot right)                                                               -- right right
                                        (Root r left rl ((height left) - (height rl)))
                                        (getRight right)
                                        ((1 + (max (height left) (height rl))) - (height (getRight right)))
                                  )
    | bf == -2 && rbf == 1 = let rl = getLeft right
                                 rr = getRight right
                             in
                                 (Root (getRoot (rl))                                                                 -- right left
                                       (Root r left (getLeft rl) ((height left) - (height (getLeft rl))))
                                       (Root (getRoot right) (getRight rl) rr ((height (getRight rl)) - (height rr)))
                                       ((max (height left) (height (getLeft rl))) - (max (height (getRight rl)) (height rr)))
                                 )
    | bf == 2 && lbf == 1 = let lr = getRight left
                            in
                                (Root (getRoot left)                                                                  -- left left
                                      (getLeft left)
                                      (Root r lr right ((height lr) - (height right)))
                                      ((height (getLeft left)) - (1 + (max (height lr) (height right))))
                                )
    | bf == 2 && lbf == -1 = let lr = getRight left
                                 ll = getLeft left
                             in
                                 (Root (getRoot lr)                                                                   -- left right
                                       (Root (getRoot left) ll (getLeft lr) ((height ll) - (height (getLeft lr))))
                                       (Root r (getRight lr) right ((height (getRight lr)) - (height right)))
                                       ((max (height ll) (height (getLeft lr))) - (max (height(getRight lr)) (height right)))
                                 )
    | otherwise = (Root r left right bf)
    where rbf = getBF right
          lbf = getBF left
                                   

makeAVL :: (Ord a) => [a] -> AVLTree a

makeAVL [] = Empty

makeAVL (x:xs) = addAVL x (makeAVL xs)

