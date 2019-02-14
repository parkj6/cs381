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

-- | A function that renders the image to HTML. Only works after you have
--   implemented `prog`. Applying `draw` to a MiniMiniLogo program will
--   produce an HTML file named MiniMiniLogo.html, which you can load in
--   your browswer to view the rendered image.
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
cmd (Pen mode) (m,p)             = ( (mode,p), Nothing )
cmd (Move x y) (m,p) | m == Down = ( (m,(x,y)), Just (p,(x,y)) )  
                     | otherwise = ( (m,(x,y)), Nothing  )           
                    

-- | Semantic function for Prog.
--
--   >>> prog (nix 10 10 5 7) start
--   ((Down,(15,10)),[((10,10),(15,17)),((10,17),(15,10))])
--
--   >>> prog (steps 2 0 0) start
--   ((Down,(2,2)),[((0,0),(0,1)),((0,1),(1,1)),((1,1),(1,2)),((1,2),(2,2))])
prog :: Prog -> State -> (State, [Line])
prog [] s     = (s, [])
prog (x:xs) s = case (cmd x s) of
                (ns, Nothing)   -> prog xs ns
                (ns, Just line) ->( fst(prog xs ns), [line] ++ snd(prog xs ns) )
                    
-- ls = last pen state 
-- ns = next State
-- lc = last coordinate
-- s = current state
--
-- * Extra credit
--

-- | This should be a MiniMiniLogo program that draws an amazing picture.
--   Add as many helper functions as you want.
amazing :: Prog
amazing = squp 20 ++ mandala 10 10

squp :: Int -> Prog
squp 0 = []
squp a = sq a a ++ squp (a-1)

mandala :: Int -> Int -> Prog
mandala _ 0 = []
mandala s r = sq s s ++ mandala (div (s*10) 11) (r-1)
-- s = # size of sq
-- r = # of recursion

sq :: Int -> Int -> Prog
sq x y = [Move 0 0, Pen Down, Move 0 y, Move x y, Move x 0, Move 0 0, Pen Up, Move x y, Pen Down, Move x (x+y), Move (x+y) (x+y), Move (x+y) y, Move x y]

sqrev :: Int -> Int -> Prog
sqrev x y = [Move 820 420, Pen Down, Move (820-x) 420, Move (820-x) (420- y), Move 820 (420-y), Move 820 420, Pen Up]
