module Let where


-- * Syntax

--  int  ::=  (any integer)
--  var  ::=  (any variable name)
--
--  expr ::= int                           -- integer literal
--        |  expr `+` expr                 -- addition
--        |  `let` var `=` expr `in` expr  -- variable declaration and binding
--        |  var                           -- variable reference

type Var = String

data Expr
   = Lit Int
   | Add Expr Expr
   | Let Var Expr Expr
   | Ref Var
  deriving (Eq,Show)

-- let x = 2+3 in x+x  ==>  10
ex1 = Let "x" (Add (Lit 2) (Lit 3))
              (Add (Ref "x") (Ref "x"))

-- let x = 2+3 in (let y = x+4 in x+y)  ==>  14
ex2 = Let "x" (Add (Lit 2) (Lit 3))
              (Let "y" (Add (Ref "x") (Lit 4))
                       (Add (Ref "x") (Ref "y")))

-- let x = (let y = 2+3 in y) in x + y  ==>  ERROR
ex3 = Let "x" (Let "y" (Add (Lit 2) (Lit 3)) (Ref "y"))
              (Add (Ref "x") (Ref "y"))

-- let x = (let x = 2 in x) + 3 in x + 4 ==> 9
ex4 = Let "x" (Add (Let "x" (Lit 2) (Ref"x")) (Lit 3))
              (Add (Ref "x") (Lit 4))

-- * Environments

type Env = Var -> Maybe Int

empty :: Env
empty = \_ -> Nothing

get :: Var -> Env -> Maybe Int
get x m  = m x

set :: Var -> Int -> Env -> Env
set x i m = \y -> if x == y then Just i else m y

exEnv :: Env
exEnv = (set "a" 3 . set "b" 4 . set "c" 5) empty


-- * Denotational semantics

sem :: Expr -> Env -> Maybe Int
sem (Lit i)     m = Just i
sem (Add l r)   m = case (sem l m, sem r m) of
                    (Just i, Just j) -> Just (i+j)
sem (Let x b e) m = case sem b m of
                      Just i   -> sem e (set x i m)
                      Nothing  -> Nothing
sem (Ref x)     m = get x m 
-- update b to update the variable
-- b = bound expression
-- e = body