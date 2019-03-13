module TypesPractice where

data Result = OK Int | Error
  deriving (Eq,Show)

isOK :: Result -> Bool
isOK (OK _) = True
isOK Error  = False

add :: Int -> Result -> Result
add i (OK j) = OK (i+j)
add _ Error  = Error

-- From the Prelude:
--   map :: (a -> b) -> [a] -> [b]
--   (.) :: (b -> c) -> (a -> b) -> a -> c

-- Write the type of the following expressions,
-- or write "type error" if it is not type correct:

-- ex1 = OK
-- ex2 = Error
-- ex3 = isOK Error
-- ex4 = isOK 3
-- ex5 = isOK OK 3
-- ex6 = isOK . OK
-- ex7 = add 3
-- ex8 = add 3 Error
-- ex9 = add 3 OK
-- ex10 = add 3 (OK 4)
-- ex11 = map OK
-- ex12 = map isOK
-- ex13 = map (add 3)
-- ex14 = map (add 3 . OK)
-- ex15 = map (add 3) . map OK
