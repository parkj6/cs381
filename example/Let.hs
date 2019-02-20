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

-- let x = 2+3 in x+x  ==>  ??
ex1 = Let "x" (Add (Lit 2) (Lit 3))
              (Add (Ref "x") (Ref "x"))

-- let x = 2+3 in (let y = x+4 in x+y)  ==>  ??
ex2 = Let "x" (Add (Lit 2) (Lit 3))
              (Let "y" (Add (Ref "x") (Lit 4))
                       (Add (Ref "x") (Ref "y")))

-- let x = (let y = 2+3 in y) in x + y  ==>  ??
ex3 = Let "x" (Let "y" (Add (Lit 2) (Lit 3)) (Ref "y"))
              (Add (Ref "x") (Ref "y"))


-- * Environments

type Env = Var -> Maybe Int

empty :: Env
empty = undefined

get :: Var -> Env -> Maybe Int
get = undefined

set :: Var -> Int -> Env -> Env
set = undefined

exEnv :: Env
exEnv = (set "a" 3 . set "b" 4 . set "c" 5) empty


-- * Denotational semantics

sem = undefined
