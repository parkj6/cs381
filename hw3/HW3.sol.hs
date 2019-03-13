module HW3 where

import Prelude hiding (Num)
import Data.List (intercalate)


-- 1. Define the abstract syntax of MiniLogo as a set of Haskell data types.

type Num   = Int
type Var   = String
type Macro = String

type Prog  = [Cmd]

data Mode = Down | Up
  deriving (Eq,Show)

data Expr = Ref Var
          | Lit Num
          | Add Expr Expr
  deriving (Eq,Show)

data Cmd = Pen Mode
         | Move Expr Expr
         | Define Macro [Var] Prog
         | Call Macro [Expr]
  deriving (Eq,Show)


-- 2. Define the MiniLogo macro line.
--
--    define line (x1,y1,x2,y2) {
--      pen up; move (x1,y1);
--      pen down; move (x2,y2);
--    }
--
line :: Cmd
line = Define "line" ["x1","y1","x2","y2"] [
  Pen Up, Move (Ref "x1") (Ref "y1"),
  Pen Down, Move (Ref "x2") (Ref "y2")]

-- 3. Use the line macro to define the MiniLogo macro nix.
--
--    define nix (x,y,w,h) {
--      call line (x, y, x+w, y+h);
--      call line (x, y+h, x+w, y);
--    }
--
nix :: Cmd
nix = Define "nix" ["x","y","w","h"] [
  Call "line" [Ref "x", Ref "y",
               Add (Ref "x") (Ref "w"),
               Add (Ref "y") (Ref "h")],
  Call "line" [Ref "x", Add (Ref "y") (Ref "h"),
               Add (Ref "x") (Ref "w"), Ref "y"]]

-- Smart constructor for move with literals. (Helper function.)
move :: Int -> Int -> Cmd
move x y = Move (Lit x) (Lit y)

-- 4. Define a Haskell function steps that generates a MiniLogo program.
steps :: Int -> Prog
steps 0 = [Pen Up, move 0 0, Pen Down]
steps n = steps (n-1) ++ [move (n-1) n, move n n]

-- 5. Define a Haskell function macros that returns the list of all macros
--    defined in a MiniLogo program.
macros :: Prog -> [Macro]
macros = concatMap macrosCmd
  where
    macrosCmd (Define m _ p) = m : macros p
    macrosCmd _              = []

-- 6. Pretty-print a MiniLogo program.
pretty :: Prog -> String
pretty p = intercalate ";\n" (map prettyC p) ++ ";"

-- Pretty print commands.
prettyC :: Cmd -> String
prettyC (Pen Down)      = "pen down"
prettyC (Pen Up)        = "pen up"
prettyC (Move x y)      = "move (" ++ prettyE x ++ ", " ++ prettyE y ++ ")"
prettyC (Define m vs p) = "define " ++ m
                       ++ " (" ++ intercalate ", " vs ++ ") {\n"
                       ++ pretty p ++ "\n}"
prettyC (Call m es)     = "call " ++ m
                       ++ " (" ++ intercalate ", " (map prettyE es) ++ ")"

-- Pretty print expressions.
prettyE :: Expr -> String
prettyE (Ref v) = v
prettyE (Lit n) = show n
prettyE (Add l r) = prettyE l ++ "+" ++ prettyE r

-- 7. Define a Haskell function that partially evaluates MiniLogo expressions.
optE :: Expr -> Expr
optE (Add (Lit x) (Lit y)) = Lit (x+y)
optE (Add l r) = let l' = optE l
                     r' = optE r
                     e' = Add l' r'
                 in if l' /= l || r' /= r then optE e' else e'
optE e = e

-- 8. Define a Haskell function that applies optE to all expressions in a
--    MiniLogo program.
optP :: Prog -> Prog
optP = map optC
  where
    optC (Move x y)      = Move (optE x) (optE y)
    optC (Define m vs p) = Define m vs (optP p)
    optC (Call m es)     = Call m (map optE es)
    optC c               = c
