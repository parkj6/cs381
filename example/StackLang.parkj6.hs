module StackLang where


import Prelude hiding (Num)


--
-- * Syntax of StackLang
--

-- Grammar for StackLang:
-- 
--    num ::= (any integer)
--   bool ::= `true`  |  `false`
--   prog ::= cmd*
--    cmd ::= num                         push a number on the stack
--         |  bool                        push a boolean on the stack
--         |  `add`                       add the top two numbers the stack
--         |  `equ`                       check whether the top two elements are equal
--         |  `if` prog `else` prog `end` if the value on the top is true
--                                        then run the first program, else run the second


-- 1. Encode the above grammar as a set of Haskell data types
--whenever we see Int, use num

-- data Prog = Seq Cmd Prog | End
-- Below is easier way to do it. 
type Prog = [Cmd]          -- New name for existing type

data Cmd
    = PushI Int         -- Input num
    | PushB Bool        -- Input T/F
    | Add               -- Popping does not require input
    | Equ               
    | IfElse Prog Prog  -- 2 Children (ProgL, ProgR)
   deriving (Eq,Show)

-- 2. Write the following StackLang program as a Haskell value:
--
--   3 4 add 5 equ
--
ex1 :: Prog
ex1 = [PushI 3, PushI 4, Add, PushI 5, Equ]


-- 3. Write a StackLang program that:
--     * checks whether 3 and 4 are equal
--     * if so, returns the result of adding 5 and 6
--     * if not, returns the value false
--    First write it in concrete syntax, then in abstract syntax as a Haskell value.
--
--    3 4 equ if 5 6 add else false end
--    3 4 equ ::= Bool
--    
ex2 :: Prog
ex2 = [PushI 3, PushI 4, Equ, IfElse [PushI 5, PushI 6, Add] [PushB False]]


-- 4. Write a Haskell function that takes two arguments x and y
--    and generates a StackLang program that adds both x and y to
--    the number on the top of the stack
-- simiilar to creating JSON

genAdd2 :: Int -> Int -> Prog
genAdd2 x y = [PushI x, PushI y, Add, Add]

-- 5. Write a Haskell function that takes a list of integers and
--    generates a StackLang program that sums them all up.

genSum :: [Int] -> Prog
genSum []     = [PushI 0]
genSum (i:is) = genSum is ++ [PushI i, Add] -- Better for Optimization
-- >>> genSum [1, 2, 3]
-- [PushI 0,PushI 3,Add,PushI 2,Add,PushI 1,Add]

--Alternate
-- genSum (i:is) = [PushI i] ++ genSum is ++ [Add]
-- >>> genSum [1, 2, 3]
-- [PushI 1,PushI 2,PushI 3,PushI 0,Add,Add,Add]




--
-- * Semantics of StackLang (now!)
--


-- 6. Identify/define a semantics domain for Cmd and for Prog.
--    Things we need:
--      * stack
--        * numbers
--        * booleans
--      * errors
--        * type error
--        * stack underflow



-- 7. Define the semantics of a StackLang command (ignore If at first).
cmd = undefined


-- 8. Define the semantics of a StackLang program.
prog = undefined


-- | Run a program on an initially empty stack.
--
--   >>> run ex2
--   Just [Right False]
--
--   >>> run (genSum [1..10])
--   Just [Left 55]
--
--   >>> run [PushN 3, Add, PushN 4]
--   Nothing
--
-- run :: Prog -> Maybe Stack
-- run p = prog p []
