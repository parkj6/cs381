Stuff we might have in the exam:

-- map: basically splits the functions and turns into list.
map :: (a->b) -> [a]->[b]
map f []     = []
map f (x:xs) = f x : map f xs
--example:
map f [2,3,4] = [f 2, f 3, f 4]


-- foldr: loop for aggregating elements in a list
foldr :: (a->b->b) -> b -> [a] -> b
foldr f y []     = y
foldr f y (x:xs) = fx (foldr f y xs)
--example:
foldr f y [2,3,4] = f 2 (f 3 (f 4 y))

filter :: (a -> Bool) -> [a] -> [a]

(.) :: (b->c) -> (a->b) -> a->c
f . g = \x -> f (g x)

data Maybe a = Nothing | Just a

data Expr = Lit Int             -- case 1
          | Plus Expr Expr      -- case 2
-- Expr = type name
-- Plus = data constructor
-- Expr Expr = types of arguments

data List a = Nil               -- a = type parameter
            | Cons a (list a)   
-- first a = reference to type parameter
-- (List a) = recursive reference to type


-- Grammar (BNF notation)
s 2 Sentence ::= n v n | s and s    
n 2 Noun     ::= cats | dogs | ducks
v 2 Verb     ::= chase | cuddle
-- note: '2' are "with in"
-- Right of '2' = syntactic category
-- n v n = nonterminal symbol
-- cats, chase etc. = terminal symbol
-- right of ::= production rules. 


-- Abstract Grammar vs. Concrete Grammar
Abstract:
t 2 Term ::= true
           | false
           | not t
           | if t t t

Concrete:
t 2 Term ::= true
           | false
           | not t
           | if t then t else t
           | ( t )

--Denotational Semantics
Valuation Function:
data Term = ...         -- abstrax Syntax, T
type Value = ...        -- semantic domain, V
sem :: Term -> Value    -- Valuation function, [[a]] : T -> V




-- Negation as failure (Prolog)
not(P) :- P, !, fail.
not(P).
