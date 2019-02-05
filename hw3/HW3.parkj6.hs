--Team: Cory Hayes (hayescor), Jong Park (parkj6), Jacob Lyons (lyonsja)

module HW3 where

import Prelude hiding (Num)
-- | A functional programming language that creates a MiniLogo language
-- | that moves pens around a cartesian plane.
-- | Syntax of MiniLogo is defined by following Grammar.
-- prog	::=	ε   |   cmd ; prog	sequence of commands

type Prog = [Cmd]

-- num	::=	(any natural number)
type Num = Int

-- var	::=	(any variable name)
type Var = String

-- macro	::=	(any macro name)
type Macro = String

-- cmd	::=	pen mode	change pen mode
-- |	move ( expr , expr )	move pen to a new position
-- |	define macro ( var* ) { prog }  	define a macro
-- |	call macro ( expr* )	invoke a macro
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
-- |	num	literal number
-- |	expr + expr	addition expression
data Expr = VAR Var
          | NUM Num
          | EXPR Expr Expr
  deriving (Eq, Show)


--define line (x1,y1,x2,y2) {
--  pen up; move (x1,y1); pen down; move (x2,y2);
--}


line = Define "line" ["x1","y1","x2","y2"][Pen Up, (Move (VAR "x1") (VAR "y1")), Pen Down, (Move (VAR "x2") (VAR "y2")) ]

--define nix (x, y, h, w){
-- call line(x,y, x+w, y+h)
-- call line(x+w, y, x, y+h)
--}

nix = Define "nix"["x", "y", "h", "w"][Call "line" [VAR "x", VAR "y", EXPR (VAR "x") (VAR "w"), EXPR (VAR "y") (VAR "h")], 
                                       Call "line" [EXPR(VAR "x")(VAR "w"), VAR "y", VAR "x", EXPR (VAR "y") (VAR "h")]]


-- steps 1
-- [Pen Up, Move (NUM 0) (NUM 0), Pen Down, Move (NUM 0) (NUM 1), Move (NUM 1) (NUM 1)]
-- 
--
-- steps 3
-- [Pen Up, Move (NUM 0) (NUM 0), Pen Down, Move (NUM 0) (NUM 1), Move (NUM 1) (NUM 1), Move (NUM 1) (NUM 2), Move (NUM 2) (NUM 2),
--  Move (NUM 2) (NUM 3), MOVE (NUM 3) (NUM 3)]
--
--

steps :: Int -> Prog
steps 0 = [Pen Up, Move (NUM 0) (NUM 0), Pen Down];
steps i = steps (i-1) ++ [Move (NUM (i-1)) (NUM i), Move (NUM i) (NUM i)]




macros :: Prog-> [Macro]
macros [] = []
macros (h:t) = case h of Define m _ _ -> [m] ++ macros t
                         _ -> macros t                    --This indentation needs to stay
             




--pretty :: Prog -> String