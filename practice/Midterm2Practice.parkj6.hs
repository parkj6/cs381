module M2practice where

-- 1. Consider the following abstract for a language for describing times.
type Hour = Int
type Minutes = Int

data Time = Midnight            -- constant time 00:00
          | Noon                -- constant time 12:00
          | AM Hour             -- time: 00:00 - 11:59
          | PM Hour             -- time: 12:00 - 23:59
          | Before Minutes Time
          | After Minutes Time
  deriving (Eq,Show)


-- |(a) Implement a denotational semantics for this language using Int as the semantic domain, where the integer represents the number of minutes since midnight. 
-- time 8:13am
-- >>> time (After 13 (AM 8))
-- 493

time :: Time -> Int
time Midnight     = 0             -- 00:00 = 0
time Noon         = 12 * 60       -- 12:00 = 720
time (AM 12)      = 0             -- Midnight
time (AM h)       = h * 60        -- Hour x 60min
time (PM h)       = h * 60 + 720  -- Hour x 60min + 12hr
time (Before m t) = time t - m
time (After  m t) = time t + m

-- |(b) Implement a revised version of this denotational semantics that checks to make sure that all hour values are between 1 and 12, and returns an error otherwise.

checkTime :: Time -> Maybe Int
checkTime Midnight      = Just 0
checkTime Noon          = Just (12 * 60)
checkTime (AM 12)       = Just 0
checkTime (AM h)        = if h >= 0 || h <= 12 then Just (h * 60) else Nothing 
checkTime (PM h)        = if h >= 0 || h <= 12 then Just (h * 60 + 720) else Nothing 
checkTime (Before m t)  = case checkTime t of
                            Just h    -> Just (h - m)
                            otherwise -> Nothing
checkTime (After m t)   = case checkTime t of
                            Just h    -> Just (h + m)
                            otherwise -> Nothing 

-- 2. Consider the following abstract syntax for a language describing movements on a 2-dimensional plane.

type Pos = (Int,Int)

data Move = JumpTo Pos     -- immediately move to the given position
          | GoUp Int       -- move vertically
          | GoRight Int    -- move horizontally
          | Seq Move Move  -- do one move followed by another
  deriving (Eq,Show)

  -- >>> move (JumpTo (2,3)) (0,0)
  -- (2,3)
  -- >>> move (JumpTo (333,3)) (0,0)
  -- (333,3)
  -- >>> move (GoUp 3) (0,0)
  -- (0,3)
  -- >>> move (GoUp 3) (0,10)
  -- (0,13)
  -- >>> move (GoRight  3) (0,10)
  -- (3,10)
  -- >>> move (GoRight 20) (0,10)
  -- (20,10)
  -- >>> move (Seq (JumpTo (2,3)) (GoRight 3)) (0,10)
  -- (5,3)
  -- >>> move (Seq (Seq (JumpTo (2,3)) (GoRight 3)) (GoUp 10)) (0,10)
  -- (5,13)

move :: Move -> Pos -> Pos
move (JumpTo p)  _     = p
move (GoUp i)    (x,y) = (x, y+i)
move (GoRight i) (x,y) = (x+i, y)
move (Seq m1 m2) p     = move m2 (move m1 p) 

-- 3. Consider the following abstract syntax for a language for building and manipulating non-nested integer lists.
-- Your task is to implement a static type system for this language. Note that the language does *not* support nested lists. 
-- That is, there are only two valid types in our language: integers and lists of integers, anything else is a type error.

data Expr = N Int           -- integer value
          | Empty           -- empty integer list
          | Sum Expr        -- sum of an integer list
          | Cons Expr Expr  -- prepend an integer to a list
  deriving (Eq,Show)

data Val = I Int | L [Int]
  deriving(Eq,Show)

expr :: Expr -> Val
expr (N i)        = I i
expr (Empty)      = L []
expr (Sum e)      = case expr e of
                      L l -> I (sum l)
                      _   -> error "SUM-thing went wrong..."
expr (Cons e1 e2) = case (expr e1, expr e2) of
                      (L l, I i) -> L (i:l)
                      (I i, L l) -> L (i:l)
                      _          -> error "CON't calculate it..."
