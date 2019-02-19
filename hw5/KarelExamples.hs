-- | This module defines some example worlds and programs for testing
--   your Karel interpreter.
module KarelExamples where

import Prelude hiding (Either(..))
import KarelSyntax
import KarelState


--
-- * Trivial worlds
--

-- | A world that is all walls.
wallWorld :: World
wallWorld = const Nothing

-- | A world that is completely empty of walls and beepers.
emptyWorld :: World
emptyWorld = const (Just 0)

-- | A robot that starts at the origin, facing north, with the
--   given number of beepers.
originBot :: Int -> Robot
originBot b = ((0,0),North,b)


--
-- * Demo world
--

-- | A 10x5 room with some beepers in a line, illustrated below.
--
--       XXXXXXXXXXXX
--       X          X
--       X          X
--       X          X
--       X @  2 3 4 X
--       X          X
--       XXXXXXXXXXXX
-- 
--   The @ symbol is coordinate (1,1).
--   Clear spaces are from (0,0) to (9,4).
--   There are beepers at the following locations:
--    * 2 beepers at (4,1)
--    * 3 beepers at (6,1)
--    * 4 beepers at (8,1)
-- 
demoWorld :: World
demoWorld (4,1) = Just 2
demoWorld (6,1) = Just 3
demoWorld (8,1) = Just 4
demoWorld (x,y) | x >= 0 && x < 10 &&
                  y >= 0 && y < 5      = Just 0
                | otherwise            = Nothing

-- | An initial robot state for the demo world. The robot starts at
--   (1,1), facing East, with 1 beeper in its bag.
demoBot :: Robot
demoBot = ((1,1),East,1)


--
-- * Program generators
--

-- | Generate a program that does the following n times:
--     1. moves in a straight line until it finds a beeper
--     2. picks it up
--     3. returns to its original position and facing.
fetcher :: Int -> Prog
fetcher n = ([("fetch",fetch)], main)
  where
    fetch = Block
      [ While (Not Beeper)   -- do until we find a beeper:
          (If (Clear Front)    -- can we move forward?
              Move             -- if yes, then do it
              Shutdown)        -- if not, shut down
      , PickBeeper ]
    main = Block
      [ Iterate n $ Block  -- repeat n times:
         [ PutBeeper         -- put a beeper down to mark our place
         , Move              -- move forward one space
         , Call "fetch"      -- go get a new beeper
         , Turn Back         -- turn around
         , Move              -- move forward one space
         , Call "fetch"      -- go back to where we started
         , Turn Back ]       -- turn back to our initial facing
      , Shutdown ]

-- | Generates a statement that moves the robot in a rectangle of
--   the given dimensions.
rectangle :: Int -> Int -> Stmt
rectangle w h = Block
  [ While (Not (Facing North)) (Turn Right)
  , Iterate h Move
  , Turn Right
  , Iterate w Move
  , Turn Right
  , Iterate h Move
  , Turn Right
  , Iterate w Move
  , Turn Right ]
