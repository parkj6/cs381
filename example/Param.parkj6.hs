-- | Illustration of static vs. dynamic scope and
--   several different parameter passing schemes.
module Param where


--
-- * Abstract syntax
--

-- | Variable names.
type Var = String

-- | Abstract syntax.
data Exp = Lit Int          -- integer literal
         | Add Exp Exp      -- addition expression
         | Let Var Exp Exp  -- variable binding
         | Ref Var          -- variable reference
         | Fun Var Exp      -- anonymous function w/ one argument
         | App Exp Exp      -- function application
  deriving (Eq,Show)


-- ** Example programs


-- | Example program that defines the successor function then uses it.
--  
--   let succ = \x -> x+1
--   in succ (succ 5)
exSucc :: Exp
exSucc = Let "succ" (Fun "x")


-- | Example program that illustrates the difference between
--   static and dynamic scope. Is the result 12 or 13?
--   Static = 12, Dynamic = 13
--
--   let z = 2 in
--   let f = (\x -> x+z) in
--   let z = 3 in
--   f 10
--
exScope :: Exp
exScope = let "z" (Lit 2)
        $ Let "f" (Fun "x" (Add (Ref "x") (Ref "z")))

-- $ is an argument, it could be replaced with ()

--
-- * Various semantics
--
--   Illustrating different scoping and parameter passing schemes.
--

-- | An environment maps variables to some type of values.
type Env a = [(Var,a)]


-- ** Dynamic scoping, call-by-value

-- | Values.
data DVal
   = DI Int      -- integers
   | DF Var Exp  -- functions
  deriving (Eq,Show)

-- | Semantic function.
dsem :: Exp -> Env DVal -> Maybe DVal
dsem (Lit i)     m = undefined
dsem (Add l r)   m = undefined
dsem (Let x b e) m = undefined
dsem (Ref x)     m = undefined
dsem (Fun x e)   m = undefined
dsem (App l r)   m = undefined

-- m is the environment at the call site (dynamic)


-- ** Static scoping, call-by-value

-- | Values.
data SVal
   = SI Int                 -- integer
   | SC (Env SVal) Var Exp  -- closure
  deriving (Eq,Show)

-- | Semantic function.
ssem :: Exp -> Env SVal -> Maybe SVal
ssem (Lit i)     m = Just (SI i)
ssem (Add l r)   m = undefined
ssem (Let x b e) m = case ssem b m of
                      Just
ssem (Ref x)     m = lookup x m
ssem (Fun x e)   m = Just (SC m x e)
ssem (App l r)   m = case (ssem l m, ssem r m) of
                      (Just (SC m' x e), Just v) -> ssem e ((x,v):m')
                      _ -> Nothing


-- look up function names on where it was defined

-- ** Static scoping, call-by-name


-- ** Static scoping, call-by-need (lazy evaluation)

