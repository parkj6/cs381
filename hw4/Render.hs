-- | A module for rendering lines as an HTML5 file containing an SVG image.
--   This can be used to visualize the denotational semantics of a MiniLogo
--   program.
--
--   NOTE: You should not change the definitions in this file!
--
module Render (Point,Line,toHTML,toGridHTML) where

import Data.List (intercalate)


-- | A point is a cartesian pair (x,y).
type Point = (Int,Int)

-- | A line is defined by its endpoints.
type Line = (Point,Point)

-- | Output a list of lines as an HTML5 file containing an SVG image.
toHTML :: [Line] -> IO ()
toHTML ls = writeFile "MiniMiniLogo.html" (header ++ content ls ++ footer)

-- | Alternate version of 'toHTML' that adds a grid to the background.
toGridHTML :: [Line] -> IO ()
toGridHTML ls = writeFile "MiniMiniLogo.html" (header ++ grid ++ content ls ++ footer)

--
-- Private definitions. All definitions below this point will not be visible
-- from within a module that imports this module.
--

scale, margin, width, height :: Int
scale  = 10
margin = 10
width  = 800
height = 400

gridStep = 5
maxX = width `div` scale
maxY = height `div` scale

gridStyle = "fill:none;stroke:lightgrey;stroke-width:1"
drawStyle = "fill:none;stroke:red;stroke-width:2"

title  = "<head><title>MiniLogo Semantics Viewer</title></head>"
view   = "<svg width='100%' viewBox='0 0 "
         ++ show (width + 2*margin) ++ " "
         ++ show (height + 2*margin) ++ "'>"
border = "<rect x='" ++ show (margin-3) ++
             "' y='" ++ show (margin-3) ++
         "' width='" ++ show (width +6) ++
        "' height='" ++ show (height+5) ++
         "' style='fill:none;stroke:black;stroke-width:2'/>"

header = unlines ["<!DOCTYPE html>", "<html>", title, "<body>", view, border]
footer = unlines ["</svg>","</body>","</html>"]

grid = unlines (map (poly gridStyle) lines)
  where lines = [ [(x,0), (x,maxY)] | x <- [0,gridStep..maxX] ]
             ++ [ [(0,y), (maxX,y)] | y <- [0,gridStep..maxY] ]

content :: [Line] -> String
content = unlines . map (poly drawStyle) . chunk

-- | A canvas-adjusted point as a string.
point :: Point -> String
point (x,y) = show xp ++ "," ++ show yp
  where xp = x*scale + margin
        yp = height - y*scale + margin

-- | Chunk a bunch of lines into sequences of connected points.
chunk :: [Line] -> [[Point]]
chunk []         = []
chunk [(p,q)]    = [[p,q]]
chunk ((p,q):ls) | q == head ps = (p:ps) : pss
                 | otherwise    = [p,q] : ps : pss
  where (ps:pss) = chunk ls

-- | Draw a sequence of connected points.
poly :: String -> [Point] -> String
poly style ps = "<polyline points='"
             ++ intercalate " " (map point ps)
             ++ "' style='" ++ style ++ "'/>"
