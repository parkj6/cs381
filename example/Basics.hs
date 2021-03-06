module Basics where

-- Don't worry about this line. It's just hiding some functions that are
-- usually imported by default, but which I'm defining my own versions of
-- in this intro file.
import Prelude hiding (length,sum,product,map,foldr)


---------------------
-- Introduce Tools --
---------------------

-- * GHCi commands
--     :help, :load, :reload, :quit, :type, :info
-- * Hoogle
-- * doctest


---------------------
-- Getting Started --
---------------------

-- In GHCi:
--  * basic data types (Bool, Int, Float)
--  * numeric and boolean operators
--  * if-then-else expressions
--  * let-expressions


---------------------
-- Basic Functions --
---------------------

-- * defining and applying functions
-- * pattern matching
-- * partial application


-- | Add an integer to itself.
double :: Int -> Int
double x = x + x

-- | Is this integer zero?
isZero :: Int -> Bool
isZero 0 = True
isZero _ = False
-- isZero x = x == 0

-- | Is this integer non-zero?
isNonZero :: Int -> Bool
isNonZero 0 = False
isNonZero _ = True
-- isNonZero x = x /= 0


-- | Computes the average of two floating point numbers.
avg :: Float -> Float -> Float
avg _ 0 = Null
avg x y = (x + y) /2

-- | Uses avg to compute half of a floating point number.
half :: Float -> Float
half = undefined


-- In GHCi:
--  * infix vs. prefix application: operators are just functions!
--    * (+) x y = x + y
--    * avg x y = x `avg` y
-- * anonymous functions


----------------------
-- Basic Data Types --
----------------------

-- * a data type definition consists of:
--   * a new type name
--   * a set of cases, each with:
--     * a data constructor
--     * zero or more arguments
-- * more pattern matching
--   * top-level and case-expressions

-- | An example data type with two cases.
data Result = OK Int | Error
  deriving (Eq,Show)    --Things that can be checked for equality (eq) show quality checking

-- | Safely divide two integers.
safeDiv :: Int -> Int -> Result
safeDiv _ 0 = Error -- <- value of type "Result"
safeDiv x y = OK (x `div` y)

-- | Get the integer from an OK result, or return 0 on an Error.
fromResult :: Result -> Int
-- fromResult (OK i) = i
-- fromResult Error  = 0
fromResult r = case r of 
                Error -> 0
                OK i  -> i

-- | Add two results.
addResults :: Result -> Result -> Result
addResults (OK i) (OK j) = OK (i + j)
addResults _      _      = Error




-- The definition of Bool in the Haskell Prelude looks like this:
--   
--   data Bool = False | True


-- The type Result is similar to the Maybe type in the Prelude:
--
--   data Maybe a = Just a | Nothing



---------------
-- Recursion --
---------------

-- * recursive data type definitions
-- * recursive functions

-- | An example of a recursive data type.
data List
   = Nil            -- empty list 
   | Cons Int List  -- non-empty list (prepend Int in to existing List)
  deriving (Eq,Show)

-- | The empty list.
empty :: List
empty = Nil

-- | The list: [2,3,4]
exList :: List
exList = Cons 2 (Cons 3 (Cons 4 Nil)) --you can use $ instead ()

-- | Compute the length of a list.
listLength :: List -> Int   
listLength Nil        = 0
listLength (Cons h t) = 1 + listLength t --heads (Int) and tails (List)
--listLength (Cons _ t) = 1 + listLength t -- still works

-- | Compute the sum of the integers in a list.
listSum :: List -> Int
listSum = undefined


-- Example evaluation:
--  Sub left with right by default (R2L works too.)
--
-- listSum (Cons 3 (Cons 4 Nil))
-- => 3 + listSum (Cons 4 Nil)
-- => 3 + (4 +listSum Nil)
-- => 3 + (4 + 0)
-- =>* 7

-- Use a *type parameter* to define lists that contain elements of any type:
--
--    data List a
--       = Nil
--       | Cons a (List a)   -- all elements must be of the *same* type
--
--    listLength :: List a -> Int
--    listSum    :: List Int -> Int
--    listSum'   :: Num a => List a -> a



-------------------
-- Haskell Lists --
-------------------

-- * Haskell's built-in list and string types
--   * cons, nil, and syntactic sugar
--   * more recursive functions

-- data [a]
--    = []         -- Nil
--    | a : [a]    -- Cons


-- The definition of String in the Haskell Prelude looks like this:
--
--   type String = [Char]


-- | Compute the length of a list.
length :: [a] -> Int
length []    = 0
length (_:t) = 1 + length t

-- | Compute the sum of an integer list.
sum :: [Int] -> Int
sum []    = 0
sum (h:t) = h + sum t

-- | Compute the product of the elements in a list.
product :: [Int] -> Int
product []    = 1
product (h:t) = h * product t -- accumulator function

allOdd :: [Int] -> Bool
allOdd []    = True
allOdd (h:t) = odd h && allOdd t


-- | Double all the elements in an integer list.
doubleAll :: [Int] -> [Int]
doubleAll []    = []
doubleAll (h:t) = (2 * h) : doubleAll t  -- prepending 2x of h into t using doubleAll

-- | Flip all of the boolean values in a boolean list.
notAll :: [Bool] -> [Bool]
notAll [] = []
notAll (h:t) = not h : notAll t

-- | Apply the even function to all elements in the list.
evenAll :: [Int] -> [Bool]
evenAll []    = []
evenAll (h:t) = even h : evenAll t

--  *Basics> :t curry
--  curry :: ((a, b) -> c) -> a -> b -> c
--  *Basics> :t uncurry
--  uncurry :: (a -> b -> c) -> (a, b) -> c
-- $ is function application
-- > zipWith ($)
----------------------------
-- Higher-Order Functions --
----------------------------

-- * map and foldr


-- | Map a function over the elements in a list.
-- input paramemter that's different from doubleAll and notAll
-- type of input has to match the type of output
map :: (a -> b) -> [a] -> [b]  --function -> list -> output list 
map f [] = []
map f (h:t) = f h : map f t
--h = element of a
--t = list of a


-- | Reimplement doubleAll using map.
doubleAll' :: [Int] -> [Int]
doubleAll' = map (2*)
doubleAll'' xs = map (2*) xs -- this is same as above


-- | Reimplement notAll using map.
notAll' :: [Bool] -> [Bool]
notAll' = map not

-- | Reimplemnt evenAll using map.
evenAll' :: [Int] -> [Bool]
evenAll' = map even 


-- | Fold an accumulator function over the elements in a list.
-- base case = b
-- input [a]
-- output b
-- binary structure ( -> -> )
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f b []    = b
foldr f b (h:t) = f h (foldr f b t)
-- h is type a

-- | Reimplement sum using foldr.
sum' :: [Int] -> Int
sum' = foldr (+) 0

-- | Reimplement product using foldr.
product' :: [Int] -> Int
product' = foldr (*) 1

-- | Reimplement length using foldr.
length' :: [a] -> Int
length' = foldr (\_ l -> 1 + l) 0
-- h = head
-- l = length
-- r = result
-- \ = I'm defining function right now (py: lambda)

-- | Reimplement allOdd using foldr
allOdd' :: [Int] -> Bool
allOdd' = foldr (\h r -> odd h && r) True --by default true


-- | Use foldr to count the True values in a list of Bools.
countTrues :: [Bool] -> Int
-- countTrues = foldr (\h r -> if h == True then ... else ... ) 0  -- pattern here
countTrues = foldr (\h r -> if h then 1 + r else r) 0  -- empty list = no trues 
