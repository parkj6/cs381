--Team: Cory Hayes (hayescor), Jong Park (parkj6), Jacob Lyons (lyonsja)

module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)

import KarelSyntax
import KarelState
import KarelExamples

-- | Valuation function for Test.
test :: Test -> World -> Robot -> Bool
test (Not t)    w r = not (test t w r) 
test (Facing c) _ r = c == (getFacing r)
test (Clear d)  w r = isClear (relativePos d r) w
test Beeper     w r = hasBeeper (getPos r) w
test Empty      _ r = isEmpty r

-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown      = \_ _ r -> Done r
stmt PickBeeper    = \_ w r -> let p = getPos r
                               in if hasBeeper p w
                                  then OK (decBeeper p w) (incBag r)
                                  else Error ("No beeper to pick at: " ++ show p)
stmt PutBeeper     = \_ w r -> let p = getPos r
                               in if test (Not Empty) w r && hasBeeper p w || 
                                     test (Not Empty) w r && test (Clear Front) w r
                                  then OK (incBeeper p w) (decBag r)
                                  else Error ("No beeper to put.") 
stmt Move          = \_ w r -> let f = getFacing r
                               in if test (Clear Front) w r
                                  then OK w (updatePos (neighbor f) r)
                                    -- new address: (neighbor (cardTurn (Front c)) (getPos r))
                                  else Error ("Blocked at: " ++ show (neighbor f (getPos r)))
stmt (Turn d)      = \_ w r -> OK w (updateFacing (cardTurn d) r)
stmt (Block [])    = \_ _ r -> Done r
stmt (Block [a])   = \d w r -> stmt a d w r
stmt (Block (a:b)) = \d w r -> onOK (stmt (Block b) d) (stmt a d w r)
stmt (If t s1 s2)  = \d w r -> if test t w r
                               then stmt s1 d w r 
                               else stmt s2 d w r
stmt (Call m)      = \d w r -> case lookup m d of
                               Just b -> stmt b d w r
                               _      -> Error ("Undefined macro: " ++ m)
stmt (Iterate i s) = \d w r -> case i of
                               0 -> Done r
                               1 -> stmt s d w r
                               i -> onOK (stmt (Iterate (i-1) s) d) (stmt s d w r)
stmt (While t s)   = \d w r -> if test t w r
                               then onOK (stmt (While t s) d) (stmt s d w r)
                               else OK w r

-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
