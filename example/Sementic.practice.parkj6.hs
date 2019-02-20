module SemanticP where

-- Example 1:
-- Consider the following language for implementing a simple counter. 
-- Statements either increment the counter by a given integer, or they reset the counter to zero. 
-- A program runs a sequence of statements on an initial counter of 0 and returns the final value of the counter.

-- i ::= (any integer)
-- s ::= inc i
--    |  reset
-- p ::= s ; p
--    |  ε

-- 1. What is a good semantic domain for statements?
-- smt :: Smt -> Int -> Int

-- 2. Implement the language in Haskell by 
-- (a) encoding the abstract syntax as a Haskell data type, 

data Smt = Inc Int
         | Reset
    deriving (Eq,Show)

type Counter = [Smt]

-- (b) implementing valuation functions for both statements and programs.

smt :: Smt -> Int -> Int
smt (Inc i) c = c + i
smt Reset   _ = 0

stmts :: [Smt] -> Int -> Int
stmts [] c = c
stmts (x:y) c = stmts y (smt x c)

cprog :: Counter -> Int
cprog p = stmts p 0



-- Example 2:
-- Consider the following command language for controlling a robot that moves in a one-dimensional space (i.e. back and forth along a line).

data Cmd = Gas | Brake | Turn
  deriving (Eq,Show)

type Prog = [Cmd]

-- The state of the robot is represented by three components: 
--     its current position on the line, 
--     its current direction, and 
--     its speed. 
-- Moving forward corresponds to increasing the position, 
-- while moving backward decreases the position.

type Pos   = Int
type Speed = Int

data Dir   = Forward | Backward
  deriving (Eq,Show)

type State = (Pos, Dir, Speed)

data Result = OK State
            | Crash Pos
    deriving (Eq,Show)

-- The commands work as follows:

--     Gas: Move in the current direction an amount equal to the current speed, then increase the speed by one. For example, if the robot is at position 5 while moving forward at a speed of 2, after executing a Gas command the robot would be at position 7 moving at a speed of 3.

--     Brake: Move in the current direction an amount equal to the current speed, then decrease the speed by one down to a minimum speed of 0. If the robot is already at speed 0, then a Brake command has no effect.

--     Turn: If the current speed of the robot is 0, then change the direction of the robot. If the speed is not 0, the robot crashes. If the robot crashes, it no longer has a speed or direction, but it does still have a position (the position it was at when it crashed).


-- 1. What is a good semantic domain for commands?

-- 2. Implement semantic functions for Cmd and Prog.
cmd :: Cmd -> State -> Result
-- cmd Gas   (p, d, 0)        = OK (p, d, 1)
cmd Gas   (p, Forward, s)  = OK (p+s, Forward,  s+1)
cmd Gas   (p, Backward, s) = OK (p-s, Backward, s+1)
cmd Brake (p, d, 0)        = OK (p, d, 0)
cmd Brake (p, Forward, s)  = OK (p+s, Forward,  s-1)
cmd Brake (p, Backward, s) = OK (p-s, Backward, s-1)
cmd Turn  (p, Forward, 0)  = OK (p,   Backward,   0)
cmd Turn  (p, Backward, 0) = OK (p,   Forward,    0)
cmd Turn  (p, _, _)        = Crash p

prog :: Prog -> State -> Result
prog []    s = OK s
prog (x:y) s = case cmd x s of
                OK s' -> prog y s'
                crash -> crash

