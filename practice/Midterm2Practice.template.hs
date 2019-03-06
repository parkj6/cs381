module M2practice where

-- 1. Consider the following abstract for a language for describing times.
type Hour = Int
type Minutes = Int

data Time = Midnight
          | Noon
          | AM Hour
          | PM Hour
          | Before Minutes Time
          | After Minutes Time
  deriving (Eq,Show)

-- 2. Consider the following abstract syntax for a language describing movements on a 2-dimensional plane.

type Pos = (Int,Int)

data Move = JumpTo Pos     -- immediately move to the given position
          | GoUp Int       -- move vertically
          | GoRight Int    -- move horizontally
          | Seq Move Move  -- do one move followed by another
  deriving (Eq,Show)

-- 3. Consider the following abstract syntax for a language for building and manipulating non-nested integer lists.

data Expr = N Int           -- integer value
          | Empty           -- empty integer list
          | Sum Expr        -- sum of an integer list
          | Cons Expr Expr  -- prepend an integer to a list
  deriving (Eq,Show)


data Type = TInt | TList | Error
    deriving (Eq, Show)

typeOf :: Expr -> Type
typeOf (N _) = TInt
typeOf Empty = TList
typeOf (Sum e) = if typeOf e == TList then TInt else Error
typeOf (Cons i l) = if typeOf i == TInt && typeOf l == TList then TList else Error
