-- | This module defines the syntax of MiniMiniLogo. It also provides
--   functions to generate programs that draw some basic shapes.
--
--   NOTE: You should not change the definitions in this file!
--
module MiniMiniLogo where

--
-- * Syntax
--

-- | A program is a sequence of commands.
type Prog = [Cmd]

-- | The mode of the pen.
data Mode = Down | Up
  deriving (Eq,Show)

-- | Abstract syntax of commands.
data Cmd = Pen Mode
         | Move Int Int
  deriving (Eq,Show)

-- | Generate a MiniMiniLogo program that draws a 2x2 box starting from the
--   specified point. Conceptually, this program looks like the following, but
--   the additions are carried out in Haskell rather than in MiniMiniLogo.
-- 
--     pen up; move (x,y);
--     pen down; move (x+2,y); move (x+2,y+2);
--               move (x,y+2); move (x,y);
--
--   >>> box 7 3
--   [Pen Up,Move 7 3,Pen Down,Move 9 3,Move 9 5,Move 7 5,Move 7 3]
--
box :: Int -> Int -> Prog
box x y = [Pen Up, Move x y, Pen Down,
           Move (x+2) y, Move (x+2) (y+2), Move x (y+2), Move x y]

-- | Generate an 'X' from (x,y) to (x+w,y+h).
--
--   >>> nix 10 10 5 7
--   [Pen Up,Move 10 10,Pen Down,Move 15 17,Pen Up,Move 10 17,Pen Down,Move 15 10]
--
nix :: Int -> Int -> Int -> Int -> Prog
nix x y w h = [Pen Up, Move x y, Pen Down, Move (x+w) (y+h),
               Pen Up, Move x (y+h), Pen Down, Move (x+w) y]

-- | Generate a MiniMiniLogo program that draws n steps starting from
--   point (x,y).
--
--   >>> steps 3 2 4
--   [Pen Up,Move 2 4,Pen Down,Move 2 5,Move 3 5,Move 3 6,Move 4 6,Move 4 7,Move 5 7]
-- 
steps :: Int -> Int -> Int -> Prog
steps n x y = [Pen Up, Move x y, Pen Down] ++ step n
  where 
    step 0 = []
    step n = step (n-1) ++ [Move (x+n-1) (y+n), Move (x+n) (y+n)]

-- | Draw an example picture. The expected output is given on the HW4
--   description page.
demo :: Prog
demo = box 7 3 ++ nix 6 6 4 3 ++ steps 3 2 4
