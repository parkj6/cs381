module Quiz2 where

data Reg = A | B                -- Register names

data Expr = Lit Int             -- Integer literal
          | Add Expr Expr       -- Addition
          | Div Expr Expr       -- Integer Division
          | Load Reg            -- Load from Register

data Stmt = Set Reg Expr        -- Set Register
          | If0 Expr Stmt Stmt  -- Conditional Statement
          | Begin [Stmt]        -- Statement Block

          
type S = (Int, Int) --(Reg A, Reg B)

expr :: (Int, Int) -> Maybe Int
--S -> V
-- Bottom up approach works better

stmt :: (Int, Int) -> Maybe (Int, Int)
-- stmt :: Int -> Int -> Maybe (Int, Int) also works
-- S -> S
-- Top down approach works better

