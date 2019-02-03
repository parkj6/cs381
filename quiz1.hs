module Quiz1 where

-- import Prelude hiding (toInt, even, and, (.))

data Result = OK Int 
			| Error

toInt :: Result -> Int
toInt Error = -1
toInt (OK x) = x
 
-- even :: Int -> Bool

add :: Int -> Int -> Int
add x y = x + y

-- (.) :: (b -> c) -> (a -> b) -> a -> c

