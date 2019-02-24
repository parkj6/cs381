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
--        |  expr `≤` expr        -- less than or equal to
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
--   end
p :: Stmt
p = undefined


--
-- * Semantics
--

-- | The current value of the register.
type Reg = Int

-- | Valuation function for expressions.
expr = undefined

-- | Non-compositional valuation function for statements.
stmt = undefined



-- ** Regaining compositionality

-- | Compute least fix point. Defined in Data.Function.
fix f = let x = f x in x

-- | Compositional valuation function for statements using least fix point.
stmt' = undefined
