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



-- 2. Write the following StackLang program as a Haskell value:
--
--   3 4 add 5 equ
--
ex1 = undefined


-- 3. Write a StackLang program that:
--     * checks whether 3 and 4 are equal
--     * if so, returns the result of adding 5 and 6
--     * if not, returns the value false
--    First write it in concrete syntax, then in abstract syntax as a Haskell value.
--
--    3 4 equ if 5 6 add else false end
--
ex2 = undefined


-- 4. Write a Haskell function that takes two arguments x and y
--    and generates a StackLang program that adds both x and y to
--    the number on the top of the stack
genAdd2 = undefined


-- 5. Write a Haskell function that takes a list of integers and
--    generates a StackLang program that sums them all up.
genSum = undefined



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
