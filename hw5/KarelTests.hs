-- | A module that contains several doctests for testing your interpreter.
--   Note that if you want the tests to pass, you'll need to reverse engineer
--   the error messages.
--
--   If you add your own tests, make sure to do that in the HW5 file, since you
--   will not submit this file.
module KarelTests where

import Prelude hiding (Either(..))

import KarelSyntax
import KarelState
import KarelSemantics
import KarelExamples


-- | Basic tests for move, turn, pick, and put.
--
--   >>> prog ([],Move) emptyWorld (originBot 0)
--   OK: ((0,1),North,0)
--
--   >>> prog ([],Block [Turn Right,Move]) emptyWorld (originBot 0)
--   OK: ((1,0),East,0)
--
--   >>> prog ([],Block [PutBeeper,PutBeeper]) emptyWorld (originBot 5)
--   OK: ((0,0),North,3)
--
--   >>> prog ([],Block [PutBeeper,PickBeeper]) emptyWorld (originBot 5)
--   OK: ((0,0),North,5)
--   
--   >>> prog ([],Block [PutBeeper,Move,Turn Back,Move,PickBeeper]) emptyWorld (originBot 5)
--   OK: ((0,0),South,5)
--
--   >>> prog ([],Move) wallWorld (originBot 0)
--   Error: Blocked at: (0,1)
--
--   >>> prog ([],Block [PutBeeper,PutBeeper]) emptyWorld (originBot 1)
--   Error: No beeper to put.
--
--   >>> prog ([],PickBeeper) emptyWorld (originBot 5)
--   Error: No beeper to pick at: (0,0)
--
--   >>> prog ([],Block [PutBeeper,Move,PickBeeper]) emptyWorld (originBot 5)
--   Error: No beeper to pick at: (0,1)


-- | Conditional tests.
--
--   >>> prog ([],If Beeper (Turn Left) (Turn Right)) emptyWorld (originBot 0)
--   OK: ((0,0),East,0)
--   
--   >>> prog ([],If (Not Beeper) (Turn Left) (Turn Right)) emptyWorld (originBot 0)
--   OK: ((0,0),West,0)
--   
--   >>> prog ([],Block [PutBeeper,If Beeper (Turn Left) (Turn Right)]) emptyWorld (originBot 3)
--   OK: ((0,0),West,2)
--
--   >>> prog ([],Block [PutBeeper,PickBeeper,If Beeper (Turn Left) (Turn Right)]) emptyWorld (originBot 3)
--   OK: ((0,0),East,3)
--
--   >>> prog ([],If (Facing North) (Turn Left) (Turn Right)) emptyWorld (originBot 0)
--   OK: ((0,0),West,0)
--
--   >>> prog ([],If (Facing South) (Turn Left) (Turn Right)) emptyWorld (originBot 0)
--   OK: ((0,0),East,0)
--
--   >>> prog ([],If (Not Empty) PutBeeper Move) emptyWorld (originBot 3)
--   OK: ((0,0),North,2)
--
--   >>> prog ([],If (Not Empty) PutBeeper Move) emptyWorld (originBot 0)
--   OK: ((0,1),North,0)
--
--   >>> prog ([],If (Clear Front) Move Shutdown) emptyWorld (originBot 0)
--   OK: ((0,1),North,0)
--
--   >>> prog ([],If (Clear Front) Move Shutdown) wallWorld (originBot 0)
--   Done: ((0,0),North,0)


-- | Test macros.
--   
--   >>> prog ([("A",Turn Right),("B",Turn Left)],Call "A") emptyWorld (originBot 0)
--   OK: ((0,0),East,0)
--
--   >>> prog ([("A",Turn Right),("B",Turn Left)],Call "B") emptyWorld (originBot 0)
--   OK: ((0,0),West,0)
--
--   >>> prog ([("A",Turn Right),("B",Turn Left)],Call "C") emptyWorld (originBot 0)
--   Error: Undefined macro: C


-- | Test looping constructs.
--
--   >>> prog ([],Iterate 8 Move) demoWorld demoBot
--   OK: ((9,1),East,1)
--
--   >>> prog ([],Iterate 9 Move) demoWorld demoBot
--   Error: Blocked at: (10,1)
--
--   >>> prog ([],While (Clear Front) Move) demoWorld demoBot
--   OK: ((9,1),East,1)
--
--   >>> prog ([],Block [Iterate 7 Move, While Beeper PickBeeper]) demoWorld demoBot
--   OK: ((8,1),East,5)


-- | Larger tests.
--
--   >>> prog (fetcher 4) demoWorld demoBot
--   Done: ((1,1),East,5)
--
--   >>> prog (fetcher 9) demoWorld demoBot
--   Done: ((1,1),East,10)
-- 
--   >>> prog (fetcher 10) demoWorld demoBot
--   Done: ((9,1),East,9)
--
--   >>> prog ([],rectangle 8 3) demoWorld demoBot
--   OK: ((1,1),North,1)
--
--   >>> prog ([],rectangle 3 8) demoWorld demoBot
--   Error: Blocked at: (1,5)
