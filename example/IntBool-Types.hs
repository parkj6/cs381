-- | A simple expression language with two types.
module IntBool where

import Prelude hiding (not,and,or)


--  Syntax of the "core" IntBool language:
--
--  int  ::=  (any integer)
--  bool ::=  true  |  false
--
--  exp  ::=  int                integer literal
--        |   exp + exp          integer addition
--        |   exp * exp          integer multiplication
--        |   exp = exp          check whether two values are equal
--        |   exp ? exp : exp    conditional expressions


-- 1. Define the abstract syntax as a Haskell data type.

data Exp = Lit Int
         | Add Exp Exp
         | Mul Exp Exp
         | Equ Exp Exp
         | If  Exp Exp Exp
  deriving (Eq,Show)


-- 2. Identify/define the semantic domain for this language
--   * what types of values can we have?
--   * how can we express this in Haskell?

-- Here are some example expressions:
--  * draw the abstract syntax trees (exercise)
--  * what should the result be?
ex1 = Mul (Lit 2) (Add (Lit 3) (Lit 4))  -- 2*(3+4)          =>  14
ex2 = Equ ex1 (Lit 10)                   -- 2*(3+4) == 10    =>  false
ex3 = If ex1 (Lit 5) (Lit 6)             -- 2*(3+4) ? 5 : 6  =>  type error

data Value
   = I Int
   | B Bool
   | TypeError
  deriving (Eq,Show)

-- Alternative semantics domain using Maybe and Either:
--
--   data Maybe a = Nothing | Just a
--   data Either a b = Left a | Right b
--
--   type Value = Maybe (Either Int Bool)
--
-- Example semantic values in both representations:
-- 
--   I 3        <=>  Just (Left 3)
--   B False    <=>  Just (Right False)
--   TypeError  <=>  Nothing
--


-- 3. Define the semantic function
sem :: Exp -> Value
sem (Lit i)    = I i
sem (Add l r)  = case (sem l, sem r) of
                   (I i, I j) -> I (i+j)
                   _ -> TypeError
sem (Mul l r)  = case (sem l, sem r) of
                   (I i, I j) -> I (i*j)
                   _ -> TypeError
sem (Equ l r)  = case (sem l, sem r) of
                   (I i, I j) -> B (i == j)
                   (B b, B c) -> B (b == c)
                   _ -> TypeError
sem (If c t e) = case sem c of
                   B True  -> sem t
                   B False -> sem e
                   _ -> TypeError


-- 4. Syntactic sugar.
--
-- Goal: extend the syntax of our language with the following operations:
--
--      * boolean literals
--      * integer negation
--      * boolean negation (not)
--      * conjunction (and)
--      * disjunction (or)
-- 
-- How do we do this? Can we do it without changing the semantics?

true, false :: Exp
true  = Equ (Lit 1) (Lit 1)
false = Equ (Lit 1) (Lit 0)

neg :: Exp -> Exp
neg e = Mul (Lit (negate 1)) e

not :: Exp -> Exp
not e = If e false true

and, or :: Exp -> Exp -> Exp
and l r = If l r false
or  l r = If l true r

-- Example program that uses our syntactic sugar.
--    not true || 3 == -3 && (true || false)
ex4 :: Exp
ex4 = or (not true) (and (Equ (Lit 3) (neg (Lit 3))) (or true false))


--
-- * Statically typed variant
--

-- 1. Define the syntax of types

data Type = TInt | TBool | TError
  deriving (Eq,Show)

-- 2. Define the typing relation.
typeOf :: Exp -> Type
typeOf (Lit _)    = TInt
typeOf (Add l r)  = case (typeOf l, typeOf r) of
                      (TInt, TInt) -> TInt
                      _ -> TError
typeOf (Mul l r)  = case (typeOf l, typeOf r) of
                      (TInt, TInt) -> TInt
                      _ -> TError
typeOf (Equ l r)  = case (typeOf l, typeOf r) of
                      (TInt, TInt) -> TBool
                      (TBool, TBool) -> TBool
                      _ -> TError
typeOf (If c t e) = case (typeOf c, typeOf t, typeOf e) of
                      (TBool, tt, te) -> if tt == te then tt else TError
                      _ -> TError


-- 3. Define the semantics of type-correct programs.
sem' :: Exp -> Either Int Bool
sem' (Lit i)    = Left i
sem' (Add l r)  = Left (evalInt l + evalInt r)
sem' (Mul l r)  = Left (evalInt l * evalInt r)
sem' (Equ l r)  = Right (sem' l == sem' r)
sem' (If c t e) = if evalBool c then sem' t else sem' e


-- | Helper function to evaluate an Exp to an Int.
evalInt :: Exp -> Int
evalInt e = case sem' e of
              Left i -> i
              _ -> error "internal error: expected Int, got something else!"

-- | Helper function to evaluate an Exp to an Bool.
evalBool :: Exp -> Bool
evalBool e = case sem' e of
               Right b -> b
               _ -> error ("internal error: expected Bool, got something else!"

-- 4. Define our interpreter.
eval :: Exp -> Value
eval e = case typeOf e of
           TInt   -> I (evalInt e)
           TBool  -> B (evalBool e)
           TError -> TypeError
