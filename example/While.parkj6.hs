-- | A single register imperative language.
module While where


--
-- * Syntax
--

--  int  ::= (any integer)
--
--  expr ::= `R`                  -- load from register
--        |  int                  -- integer literal
--        |  expr `+` expr        -- addition expression
--
-- Separating the test to avoid type error
--  test ::=  expr `≤` expr       -- less than or equal to
--
--  stmt ::= `R :=` expr          -- set register
--        |  `while` expr stmt    -- while loop
--        |  `begin` stmt* `end`  -- statement block

data Expr
   = Get
   | Lit Int
   | Add Expr Expr
   | LTE Expr Expr
  deriving (Eq,Show)

data Stmt
   = Set Expr
   | While Expr Stmt
   | Begin [Stmt]
  deriving (Eq,Show)


-- Example program:
--   begin
--     R := 1
--     while R <= 100
--       R := R + R
-- --   end
-- p :: Stmt
-- p = Begin [
--   Set (Lit 1),
--   While (LTE Get (Lit 100))
--     (Set (Add Get Get))
-- ]

--
-- * Semantics
--

-- | The current value of the register.
type Reg = Int

-- | Valuation function for expressions.
expr :: Expr -> Reg -> Int
expr Get       = \s -> s
expr (Lit i)   = \s -> i
expr (Add l r) = \s -> expr l s + expr r s
-- Everything on right side is semantic (\s)
-- s represent current value of the register

-- | Valuation function for tests.
test :: Expr -> Reg -> Bool
test (LTE l r) = \s -> expr l s <= expr r s

-- | Non-compositional valuation function for statements.
stmt :: Stmt -> Reg -> Reg
stmt (Set e)     = \s -> expr e s
stmt (While c b) = \s -> if test c s 
                         then stmt (While c b) (stmt c s)  -- fine in interpreter
                         else s
stmt (Begin l)   = \s -> stmts l s
-- stmt (Begin l)   = foldl (flip stmt) s l   --easier way to do above.
-- could also replace \s with s
-- e=expr c=Condition, b=body, l=list-of-expr

stmts :: [Stmt] -> Reg -> Reg
stmts []     = \r -> r
stmts (s:ss) = stmts ss (stmt s r) 

-- ** Regaining compositionality

-- | Compute least fix point. Defined in Data.Function.
-- fix :: (t->t) -> t
fix f = let x = f x in x

-- | Compositional valuation function for statements using least fix point.
stmt' :: Stmt -> Reg -> Reg
stmt' (Set e)    = \s -> expr e s
stmt' (While c b) = fix (\f s -> if test c s
                                then f (stmt b s) -- better for sementics.
                                else s)
stmt' (Begin l)  = \s -> foldl (flip stmt) s l


--denotational sementics = map to mathmathical values
