-- | This module defines the syntax of the Karel language.
module KarelSyntax where


-- | A Karel program is a list of macro definitions and a statement to
--   use as the body of the "main" function.
type Prog = (Defs,Stmt)

-- | A macro name.
type Macro = String

-- | A list of macro definitions.
type Defs = [(Macro,Stmt)]

-- | Cardinal directions.
data Card = North | South | East  | West  deriving (Eq,Show)

-- | Directions relative to the current facing.
data Dir  = Front | Back  | Right | Left  deriving (Eq,Show)

-- | Environment queries.
data Test = Not    Test   -- boolean negation
          | Facing Card   -- am I facing the given cardinal direction?
          | Clear  Dir    -- can I move in the given relative direction?
          | Beeper        -- is there a beeper here?
          | Empty         -- is my beeper bag empty?
  deriving (Eq,Show)

-- | Statements.
data Stmt = Shutdown                 -- end the program
          | Move                     -- move forward
          | PickBeeper               -- take a beeper
          | PutBeeper                -- leave a beeper
          | Turn    Dir              -- rotate in place
          | Call    Macro            -- invoke a macro
          | Iterate Int  Stmt        -- fixed repetition loop
          | If      Test Stmt Stmt   -- conditional branch
          | While   Test Stmt        -- conditional loop
          | Block   [Stmt]           -- statement block
  deriving (Eq,Show)
