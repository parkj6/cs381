module SemanticsPractice where

--
-- * Example 1
--

data Stmt = Inc Int
          | Reset
  deriving (Eq,Show)

type CounterProg = [Stmt]

-- Semantic domain for Stmt: Int -> Int

stmt :: Stmt -> Int -> Int
stmt (Inc i) c = c+i
stmt Reset   _ = 0

stmts :: [Stmt] -> Int -> Int
stmts []     c = c
stmts (s:ss) c = stmts ss (stmt s c)

cprog :: CounterProg -> Int
cprog p = stmts p 0


--
-- * Example 2
--

data Cmd = Gas
         | Brake
         | Turn
  deriving (Eq,Show)

type Prog = [Cmd]

type Pos   = Int
type Speed = Int

data Dir = Forward | Backward
  deriving (Eq,Show)

type State = (Pos, Speed, Dir)

data Result = OK State
            | Crash Pos
  deriving (Eq,Show)

-- Semantic domain for Cmd: State -> Result

cmd :: Cmd -> State -> Result
cmd Gas   (p, s, Forward)  = OK (p+s, s+1, Forward)
cmd Gas   (p, s, Backward) = OK (p-s, s+1, Backward)
cmd Brake (p, 0, d)        = OK (p, 0, d)
cmd Brake (p, s, Forward)  = OK (p+s, s-1, Forward)
cmd Brake (p, s, Backward) = OK (p-s, s-1, Backward)
cmd Turn  (p, 0, Forward)  = OK (p, 0, Backward)
cmd Turn  (p, 0, Backward) = OK (p, 0, Forward)
cmd Turn  (p, _, _)        = Crash p

-- | Semantics of programs.
--
--   >>> prog [Gas, Gas, Gas, Brake] (0,0,Forward)
--   OK (6,2,Forward)
--
--   >>> prog [Gas, Brake, Gas, Brake] (0,0,Backward)
--   OK (-2,0,Backward)
--
--   >>> prog [Gas, Gas, Brake, Turn, Gas, Gas] (0,0,Forward)
--   Crash 3
--
--   >>> prog [Gas, Gas, Brake, Turn] (0,0,Forward)
--   Crash 3
--
--   >>> prog [Gas, Gas, Brake, Brake, Turn, Gas, Gas] (0,0,Forward)
--   OK (3,2,Backward)
--
prog :: Prog -> State -> Result
prog []     s = OK s
prog (c:cs) s = case cmd c s of
                  OK s' -> prog cs s'
                  crash -> crash
