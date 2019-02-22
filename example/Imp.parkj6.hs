-- | A small imperative programming language.
module Imp where

import Data.Map (Map,fromList,lookup,insert)
import Data.Maybe (fromJust)
import Prelude hiding (lookup)


--
-- * Abstract syntax
--

-- | Variables.
type Var = String

-- | Abstract syntax of expressions.
--
--     expr  ::=  int
--            |   expr + expr
--            |   expr ≤ expr
--            |   `not` expr
--            |   var
--
data Expr = Lit Int        -- literal integer
          | Add Expr Expr  -- integer addition
          | LTE Expr Expr  -- less than or equal to
          | Not Expr       -- boolean negation
          | Ref Var        -- variable reference
  deriving (Eq,Show)

-- | Abstract syntax of statements.
--
--     stmt  ::=  var := expr
--            |   `if` expr stmt `else` stmt
--            |   `while` expr stmt
--            |   { stmt* }
--
data Stmt = Bind Var Expr
          | If Expr Stmt Stmt
          | While Expr Stmt
          | Block [Stmt]
  deriving (Eq,Show)

-- | Abstract syntax of types.
--     
--     type  ::=  `int`  |  `bool`
--
data Type = TInt | TBool
  deriving (Eq,Show)

-- | Abstract syntax of declarations.
--
--     decl  ::=  var : type
--
type Decl = (Var,Type)

-- | Abstract syntax of programs.
--
--     prog  ::=  decl* `begin` stmt
--
data Prog = P [Decl] Stmt
  deriving (Eq,Show)

-- | Example program: sum all of the numbers from 1 to 100.
--
--     sum : int
--     n : int
--     begin {
--       sum := 0
--       n := 1
--       while n <= 100 {
--         sum := sum + n
--         n := n + 1
--       }
--     }
ex1 :: Prog
ex1 = P [("sum", TInt),("n",Tint)]
      Block []
      
-- | Example program with a type error.
--
--     x : int
--     begin
--       x := 3 <= 4
--
ex2 :: Prog
ex2 = undefined


--
-- * Type system
--

-- | Variable environments. An environment maps variable names to the
--   things those variable names are bound to. During typing, each name
--   will be bound to a type, while during evaluation (semantics), each
--   name will be bound to a value.
type Env a = Map Var a

-- | Typing relation for expressions. We need an environment to lookup
--   the names of variable references (last case). We use a Maybe to
--   represent the fact that typing might fail, for example, if we get
--   a type error or if a variable is not in the environment.
typeExpr :: Expr -> Env Type -> Maybe Type
typeExpr (Lit _)   _ = Just TInt
typeExpr (Add l r) m = case (typeExpr l m, typeExpr r m) of
                         (Just TInt, Just TInt) -> Just TInt
                         _                      -> Nothing
typeExpr (LTE l r) m = case (typeExpr l m, typeExpr r m) of
                         (Just TInt, Just TInt) -> Just TBool
                         _                      -> Nothing
typeExpr (Not e)   m = case typeExpr e m of
                         Just TBool -> Just TBool
                         _          -> Nothing
typeExpr (Ref v)   m = lookup v m


-- | Type checking statements. Note that the return type here is just a
--   Boolean value since a statement doesn't have a type. The Boolean
--   value indicates whether or not the statement is type correct (i.e.
--   this function implements type checking of statements).
-- 
--   Also note that when we type check a while loop, we do not execute
--   the loop several times; we just check that the type of the condition
--   is a Bool, and check that the body type checks. If both of those are
--   true, then we know that the while loop cannot produce a type error
--   without having to consider each iteration. This is where the benefits
--   of static type checking start to become more clear. With dynamic
--   typing we would have to check the types in the loop body on each
--   iteration, whereas with static typing we just check the loop body
--   once.
typeStmt :: Stmt -> Env Type -> Bool
typeStmt (Bind v e)   m = undefined
typeStmt (If c st se) m = undefined
typeStmt (While c sb) m = undefined
typeStmt (Block ss)   m = undefined


-- | Type checking programs. The 'fromList' function is from the
--   Data.Map module. It builds a map from a list of pairs, thus
--   initializing our typing environment.
typeProg :: Prog -> Bool
typeProg (P ds s) = typeStmt s (fromList ds)


--
-- * Semantics
--

-- | The basic values in our language.
type Val = Either Int Bool

-- | Semantics of type-correct expressions. Note that since we assume the
--   expression is statically type correct (otherwise it would have failed
--   type checking and we never try to evaluate it), we do not need to
--   explicitly represent the error case with a Maybe type. Also note that
--   our environment now contains the *value* that each name is bound to.
--   Since expressions can refer to variables but not change them, the
--   environment is read-only (i.e. it's an input but not an output of the
--   function).
evalExpr :: Expr -> Env Val -> Val
evalExpr (Lit i)   _ = Left i
evalExpr (Add l r) m = Left (evalInt l m + evalInt r m)
evalExpr (LTE l r) m = Right (evalInt l m <= evalInt r m)
evalExpr (Not e)   m = Right (not (evalBool e m))
evalExpr (Ref x)   m = case lookup x m of
                         Just v  -> v
                         Nothing -> error "internal error: undefined variable"

-- | Helper function to evaluate an expression to an integer. Note that
--   in all cases, we should only get an "internal error" if we try to
--   evaluate an expression that didn't pass the static type checker we
--   wrote above.
evalInt :: Expr -> Env Val -> Int
evalInt e m = case evalExpr e m of
                Left i  -> i
                Right _ -> error "internal error: expected Int got Bool"

-- | Helper function to evaluate an expression to a Boolean.
evalBool :: Expr -> Env Val -> Bool
evalBool e m = case evalExpr e m of
                 Right b -> b
                 Left _  -> error "internal error: expected Bool got Int"

-- | Semantics of statements. Statements update the bindings in the
--   environment, so the semantic domain is 'Env Val -> Env Val'. The
--   bind case is the case that actually changes the environment. The
--   other cases should look similar to other examples you've seen.
evalStmt :: Stmt -> Env Val -> Env Val
evalStmt (Bind x e)   m = insert x (evalExpr e m) m
evalStmt (If c st se) m = if evalBool c m
                          then evalStmt st m
                          else evalStmt se m
evalStmt (While c sb) m = if evalBool c m
                          then evalStmt (While c sb) (evalStmt sb m)
                          else m
evalStmt (Block ss)   m = evalStmts ss m

-- | Helper function to evaluate a list of statements. We could also
--   have used 'foldl' here.
evalStmts :: [Stmt] -> Env Val -> Env Val
evalStmts []     m = m
evalStmts (s:ss) m = evalStmts ss (evalStmt s m)

-- | Semantics of programs. This runs a program with an initial
--   environment where all integer variables are initialized to 0, and
--   all Boolean variables are initialized to false.
evalProg :: Prog -> Env Val
evalProg (P ds s) = evalStmt s m
  where
    m = fromList (map (\(x,t) -> (x, init t)) ds)
    init TInt  = Left 0
    init TBool = Right False

-- | Type check and then run a program.
runProg :: Prog -> Maybe (Env Val)
runProg p = if typeProg p then Just (evalProg p)
                          else Nothing
