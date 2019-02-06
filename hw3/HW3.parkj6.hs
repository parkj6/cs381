--Team: Cory Hayes (hayescor), Jong Park (parkj6), Jacob Lyons (lyonsja)

module HW3 where
-- | A functional programming language that creates a MiniLogo language
-- | that moves pens around a cartesian plane.
-- | Syntax of MiniLogo is defined by following Grammar.

-- | Part 1.
-- | Define the abstract syntax of MiniLogo as a set of Haskell data types. 
-- | You should use built-in types for num, var, and macro. 
-- | (If you want to define a type Num, you will have to hide that name from the Prelude).
import Prelude hiding (Num)
import Data.List (intersperse)

-- prog	::=	ε   |   cmd ; prog	sequence of commands
type Prog = [Cmd]

-- num	::=	(any natural number)
-- var	::=	(any variable name)
-- macro	::=	(any macro name)
type Num = Int
type Var = String
type Macro = String

-- cmd	::=	pen mode	change pen mode
--        |	move ( expr , expr )	move pen to a new position
--        |	define macro ( var* ) { prog }  	define a macro
--        |	call macro ( expr* )	invoke a macro
data Cmd
    = Pen Mode                 -- change pen mode
    | Move Expr Expr           -- Move pen
    | Define Macro [Var] Prog  -- Define a macro
    | Call Macro [Expr]        -- Invoke a macro
    deriving(Eq, Show)

-- mode	::=	down   |   up	pen status
data Mode = Up | Down
  deriving (Eq, Show)

-- expr	::=	var	variable reference
--        |	num	literal number
--        |	expr + expr	addition expression
data Expr = VAR Var
          | NUM Num
          | EXPR Expr Expr
  deriving (Eq, Show)

-- | Task 2
-- | Define a MiniLogo macro line (x1,y1,x2,y2) that (starting from anywhere on the canvas)
-- | draws a line segment from (x1,y1) to (x2,y2).

--define line (x1,y1,x2,y2) {
--  pen up; move (x1,y1); pen down; move (x2,y2);
--}
line = Define "line" ["x1","y1","x2","y2"][Pen Up, (Move (VAR "x1") (VAR "y1")), Pen Down, (Move (VAR "x2") (VAR "y2"))]

-- | Task 3
-- | Use the line macro you just defined to define a new MiniLogo macro nix (x,y,w,h)
-- | that draws a big “X” of width w and height h, starting from position (x,y). 
-- | Your definition should not contain any move commands.

--define nix (x, y, h, w){
-- call line(x,y, x+w, y+h)
-- call line(x+w, y, x, y+h)
--}
nix = Define "nix" ["x", "y", "h", "w"][Call "line" [VAR "x", VAR "y", EXPR (VAR "x") (VAR "w"), EXPR (VAR "y") (VAR "h")], 
                                       Call "line" [EXPR(VAR "x")(VAR "w"), VAR "y", VAR "x", EXPR (VAR "y") (VAR "h")]]

-- | Task 4
-- | Define a Haskell function steps :: Int -> Prog that constructs a MiniLogo program
-- | that draws a staircase of n steps starting from (0,0). 
--
-- >>> steps 1
-- [Pen Up, Move (NUM 0) (NUM 0), Pen Down, Move (NUM 0) (NUM 1), Move (NUM 1) (NUM 1)]
--
-- >>> steps 3
-- [Pen Up, Move (NUM 0) (NUM 0), Pen Down, Move (NUM 0) (NUM 1), Move (NUM 1) (NUM 1), Move (NUM 1) (NUM 2), Move (NUM 2) (NUM 2), Move (NUM 2) (NUM 3), MOVE (NUM 3) (NUM 3)]
--
steps :: Int -> Prog
steps 0 = [Pen Up, Move (NUM 0) (NUM 0), Pen Down];
steps i = steps (i-1) ++ [Move (NUM (i-1)) (NUM i), Move (NUM i) (NUM i)]



-- | Task 5
-- | Define a Haskell function macros :: Prog -> [Macro] that returns a list of the names
-- | of all of the macros that are defined anywhere in a given MiniLogo program. 
-- | Don’t worry about duplicates—if a macro is defined more than once, 
-- | the resulting list may include multiple copies of its name.

macros :: Prog-> [Macro]
macros [] = []
macros (h:t) = case h of Define m _ _ -> [m] ++ macros t
                         _            -> macros t         --This indentation needs to stay
             


-- | Task 6
-- | Define a Haskell function pretty :: Prog -> String that pretty-prints a MiniLogo program.
-- | That is, it transforms the abstract syntax (a Haskell value) into nicely formatted 
-- | concrete syntax (a string of characters). 
-- | Your pretty-printed program should look similar to the example programs given above; 
-- | however, for simplicity you will probably want to print just one command per line.



extractCmd :: Cmd -> String
extractCmd (Pen s) = "\tpen " ++ if s == Up then "up;" else "down;"                   --Complete
extractCmd (Move e1 e2) = "\tmove (" ++ expandExprList [e1] ++ "," ++ expandExprList [e2] ++ ");" --Complete
extractCmd (Define m vars p) = "define " ++ m ++ "(" ++ expandVarList vars ++ ") {\n" ++ pretty p ++ "}"
extractCmd (Call m exprs) = "\tcall " ++ m ++ " (" ++ expandExprList exprs ++ ");"     --Complete

pretty :: Prog -> String
pretty (x:[]) = extractCmd x ++ "\n"
pretty (x:xs) = extractCmd x ++ "\n" ++ pretty xs

-- Takes [Expr] in the form [VAR "x", VAR "y", EXPR (VAR "x") (VAR "w"), EXPR (VAR "y") (VAR "h")]
-- Returns String in the form "x,y,x+w,y+h"
expandExprList :: [Expr] -> String
expandExprList (h:[]) = case h of (NUM n) -> show n 
                                  (VAR v) -> v 
                                  (EXPR (VAR e1) (VAR e2)) -> e1 ++ "+" ++ e2
expandExprList (h:t) = case h of (NUM n) -> show n ++ "," ++ expandExprList t
                                 (VAR v) -> v ++ "," ++ expandExprList t
                                 (EXPR (VAR e1) (VAR e2)) -> e1 ++ "+" ++ e2 ++ "," ++ expandExprList t

--Complete. Works
-- Takes something in the form ["x1","y1","x2","y2"]
-- Returns in the form "x1,y1,x2,y2"
expandVarList :: [Var] -> String
expandVarList (h:[]) = h
expandVarList (h:t)  = h ++ "," ++ expandVarList t


-- optE :: Expr -> Expr
-- expandExprList (h:[]) = case h of (NUM n) -> show n 
--                                   (VAR v) -> v 
--                                   (EXPR (VAR e1) (VAR e2)) -> e1 ++ "+" ++ e2