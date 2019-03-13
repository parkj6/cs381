-- 
-- * Time Language
--

type Hour = Int
type Minutes = Int

data Time = Midnight
          | Noon
          | AM Hour
          | PM Hour
          | Before Minutes Time
          | After Minutes Time
  deriving (Eq,Show)

noon :: Int
noon = 12 * 60

time :: Time -> Int
time Midnight     = 0
time Noon         = noon
time (AM 12)      = 0
time (AM h)       = h * 60
time (PM 12)      = noon
time (PM h)       = noon + h * 60
time (Before m t) = time t - m
time (After m t)  = time t + m

checkHour :: Hour -> Bool
checkHour h = h >= 0 && h <= 12

time' :: Time -> Maybe Int
time' Midnight     = Just 0
time' Noon         = Just noon
time' (AM 12)      = Just 0
time' (AM h)       = if checkHour h then Just (h * 60) else Nothing
time' (PM 12)      = Just noon
time' (PM h)       = if checkHour h then Just (noon + h * 60) else Nothing
time' (Before m t) = case time' t of
                       Just i -> Just (i - m)
                       Nothing -> Nothing
time' (After m t)  = case time' t of
                       Just i -> Just (i + m)
                       Nothing -> Nothing


--
-- * Move Language
--

type Pos = (Int,Int)

data Move = JumpTo Pos     -- immediately move to the given position
          | GoUp Int       -- move vertically
          | GoRight Int    -- move horizontally
          | Seq Move Move  -- do one move followed by another
  deriving (Eq,Show)

move :: Move -> Pos -> Pos
move (JumpTo p)  _     = p
move (GoUp i)    (x,y) = (x, y+i)
move (GoRight i) (x,y) = (x+i, y)
move (Seq m1 m2) p     = move m2 (move m1 p)


--
-- * List Language
--

data Expr = N Int           -- integer value
          | Empty           -- empty integer list
          | Sum Expr        -- sum of an integer list
          | Cons Expr Expr  -- prepend an integer to a list
  deriving (Eq,Show)

data Type = TInt | TList | Error

typeOf :: Expr -> Type
typeOf (N _)      = TInt
typeOf Empty      = TList
typeOf (Sum e)    = case typeOf e of
                      TList -> TInt
                      _ -> Error
typeOf (Cons h t) = case (typeOf h, typeOf t) of
                      (TInt, TList) -> TList
                      _ -> Error


-- Here's a denotational semantics that assumes the expression is type correct:

data Value = I Int | L [Int]
  deriving (Eq,Show)

eval :: Expr -> Value
eval (N i)      = I i
eval Empty      = L []
eval (Sum e)    = case eval e of
                    L is -> I (sum is)
                    _ -> error "internal error: type error on sum"
eval (Cons h t) = case (eval h, eval t) of
                    (I i, L is) -> L (i:is)
                    _ -> error "internal error: type error on cons"
