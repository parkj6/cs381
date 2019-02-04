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

sub :: Int -> Int -> Int
sub x y = x - y

pos :: Int -> Int -> Bool
pos x y | x-y > 0 = True
        | otherwise = False

test :: Int -> Bool -> Int
test x True = x
test x False = -127

-- (.) :: (b -> c) -> (a -> b) -> a -> c
