module HW4 where

import MiniMiniLogo
import Render


--
-- * Semantics of MiniMiniLogo
--

-- NOTE:
--  * MiniMiniLogo.hs defines the abstract syntax of MiniMiniLogo and some
--    functions for generating MiniMiniLogo programs. It contains the type
--    definitions for Mode, Cmd, and Prog.
--  * Render.hs contains code for rendering the output of a MiniMiniLogo
--    program in HTML5. It contains the types definitions for Point and Line.

-- | A type to represent the current state of the pen.
type State = (Mode,Point)

-- | The initial state of the pen.
start :: State
start = (Up,(0,0))

-- | A function that uses the semantic function that you will implement
--   to render the image. Once your semantic function is working, you should
--   be able to apply `draw` to a MiniMiniLogo program, then load the file
--   MiniMiniLogo.html in your browswer to view the rendered image.
draw :: Prog -> IO ()
draw p = let (_,ls) = prog p start in toHTML ls


-- Semantic domains:
--   * Cmd:  State -> (State, Maybe Line)
--   * Prog: State -> (State, [Line])


-- | Semantic function for Cmd.
--   
--   >>> cmd (Pen Down) (Up,(2,3))
--   ((Down,(2,3)),Nothing)
--
--   >>> cmd (Pen Up) (Down,(2,3))
--   ((Up,(2,3)),Nothing)
--
--   >>> cmd (Move 4 5) (Up,(2,3))
--   ((Up,(4,5)),Nothing)
--
--   >>> cmd (Move 4 5) (Down,(2,3))
--   ((Down,(4,5)),Just ((2,3),(4,5)))
--
cmd :: Cmd -> State -> (State, Maybe Line)
cmd (Pen Up)   (_,p) = ((Up,  p), Nothing)
cmd (Pen Down) (_,p) = ((Down,p), Nothing)
cmd (Move x y) (d,p) = ((d,q), l)
  where
    q = (x,y)
    l = case d of
          Down -> Just (p,q)
          Up   -> Nothing


-- | Semantic function for Prog.
--
--   >>> prog (nix 10 10 5 7) start
--   ((Down,(15,10)),[((10,10),(15,17)),((10,17),(15,10))])
--
--   >>> prog (steps 2 0 0) start
--   ((Down,(2,2)),[((0,0),(0,1)),((0,1),(1,1)),((1,1),(1,2)),((1,2),(2,2))])
--
prog :: Prog -> State -> (State, [Line])
prog []     s = (s,[])
prog (c:cs) s = case cmd c s of
  (s',Nothing) -> prog cs s'
  (s',Just l)  -> let (s'',ls) = prog cs s' in (s'',l:ls)
