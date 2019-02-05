--Team: Cory Hayes (hayescor), Jong Park (parkj6), Jacob Lyons (lyonsja)

module HW3 where

-- | A functional programming language that creates a MinLogo language
-- | that moves pens around a cartesian plane.

-- | Syntax of MiniLogo is defined by following Grammar.

-- prog	::=	ε   |   cmd ; prog	sequence of commands
type Prog = [Cmd]

-- num	::=	(any natural number)
data Num = Int
  deriving (Eq,Show)
-- var	::=	(any variable name)
data Var = variable
  deriving (Eq,Show)
-- macro	::=	(any macro name)
data Macro = Prog
  deriving (Eq,Show)

-- cmd	::=	pen mode	change pen mode
-- |	move ( expr , expr )	move pen to a new position
-- |	define macro ( var* ) { prog }  	define a macro
-- |	call macro ( expr* )	invoke a macro
data Cmd
    = Pen Mode          -- change pen mode
    | Move Expr Expr    -- Move pen
    | Macro [Var] Prog  -- Define a macro
    | Call Macro [Expr] -- Invoke a macro

-- mode	::=	down   |   up	pen status
data Mode = Up | Down
  deriving (Eq, Show)

-- expr	::=	var	variable reference
-- |	num	literal number
-- |	expr + expr	addition expression
data Expr = Int
          | And Expr Expr
  deriving (Eq, Show)
