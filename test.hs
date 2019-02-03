-- Example materials from schoolofhaskell.com

lista = [1,2,3,4,5,6,7,8,9,10]

-- sum 
lst = [2,3,5,7,11]

total = sum (map (3*) lst)

tot = print total

-- Compute the sum if the integers from 1 to n.
sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n - 1)

fac = print (sumtorial 10)


-- Sum the pair
sumPair :: (Int, Int) -> Int
sumPair (x,y) = x + y

-- Nested pattern to sum every 2
sumEveryTwo :: [Integer] -> [Integer]
sumEveryTwo []     = []         -- Do nothing (empty list)
sumEveryTwo (x:[]) = []         -- Do nothing (1 element on list)
sumEveryTwo (x:(y:zs)) = (x + y) : sumEveryTwo zs


-- even numbers gets /2, odd num *3+1
hailstone :: Integer -> Integer
hailstone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise      = 3 * n + 1
  
-- adding more stuff to hailstone list
hailstoneSeq :: Integer -> [Integer]
hailstoneSeq 1 = [1]
hailstoneSeq n = n : hailstoneSeq (hailstone n)

-- Compute the length of a list of Integers
intListLength :: [Integer] -> Integer
intListLength []    = 0
intListLength (_:t) = 1 + intListLength t

-- The number of hailstone steps required to reach 1 from given number
hailstoneLen :: Integer -> Integer
hailstoneLen n = intListLength (hailstoneSeq n) - 1

---------------------
-- End of lesson 1 --
---------------------

-- Creating our own enumeration types
data Thing = Shoe
                       | Ship
                       | SealingWax
                       | Cabbage
                       | King
      deriving Show

isSmall :: Thing -> Bool
isSmall Shoe                = True
isSmall SealingWax  = True
isSmall Cabbage         = True
isSmall _                      = False

isBig :: Thing -> Bool
isBig a = not (isSmall a)


-- Enmuration with more stuff
data FailableDouble = Failure
                                          | OK Double
                         deriving Show

a = Failure
b = OK 3.14
zero = 0

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)


-- Multiple algebraic data types
data AlgDataType = Constr1 Int String
                                    | Constr2 Int
                                    | Constr3 Int String Double
                                    | Constr4
                 
-- more than 1 argument
data Person  = Person String Int Thing
        deriving Show
        
-- Example humans
brent :: Person
brent = Person "Brent" 30 SealingWax

stan :: Person
stan = Person "Stan" 94 Cabbage

-- Just their age
getAge :: Person -> Int
getAge (Person _ a _ ) = a

-- Pattern matching
baz :: Person -> String
baz p@(Person n _ _) = "The name field of (" ++ show p ++ ") is " ++ n

-- >>> baz stan
-- "The name field of (Person \"Stan\" 94 Cabbage) is Stan"

-- >>> putStrLn (baz brent)
-- The name field of (Person "Brent" 30 SealingWax) is Brent

checkFav :: Person -> String
checkFav (Person n _ Cabbage) = "My Cabbages!"
checkFav (Person n _ _) = "I Don't really give a crap, " ++ n

-- Following defines what can be used as a pattern:
-- pat ::= _                                                        -- underscore is a pattern
--             | var                                                -- variable is a pattern
--             | var @ (pat)                                        -- specifies @pat as pattern
--             |(Constructor pat1 pat2 ... patn) -- Constructor name followed by a sequence of patterns is a pattern.


-- Case pattern matching 
n = case "Hello" of
  []      -> 3
  ('H':s) -> length s
--     _               -> 7             -- this is else case, but errors since it matches all.
-- n = 4 because n = length ('ello')

-- Recursive Ddata Types
data IntList = Empty | Cons Int IntList

intListProd :: IntList -> Int
intListProd Empty = 1
intListProd (Cons x xs) = x * intListProd xs

-- Basic Binary Tree
data Tree = Leaf Char 
                    | Node Tree Int Tree
   deriving Show
        
tree :: Tree
tree = Node (Leaf 'x') 1 (Node (Leaf 'y') 2 (Leaf 'z'))









