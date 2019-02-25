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
stmt Shutdown      _ _ r = Done r
stmt PickBeeper    _ w r = let p = getPos r
                        in if hasBeeper p w
                              then OK (decBeeper p w) (incBag r)
                              else Error ("No beeper to pick at: " ++ show p)
stmt PutBeeper     _ w r = let p = getPos r
                        in if test (Not Empty) w r && hasBeeper p w || 
                              test (Not Empty) w r && test (Clear Front) w r
                              then OK (incBeeper p w) (decBag r)
                              else Error ("No beeper to put.") 
stmt Move          _ w r = let f = getFacing r
                        in if test (Clear Front) w r
                              then OK w (updatePos (neighbor f) r)
                                    -- new address: (neighbor (cardTurn (Front c)) (getPos r))
                              else Error ("Blocked at: " ++ show f )
stmt (Turn d)      _ w r = OK w (updateFacing (cardTurn d) r)
stmt (Block (a:b)) _ _ r = undefined
stmt (If t s1 s2)  d w r = if test t w r
                           then stmt s1 d w r 
                           else stmt s2 d w r
stmt (Call m)      d w r = case lookup m d of
                           Just b -> stmt b d w r
                           _      -> Error ("Undefined Macro: " ++ m)
stmt (Iterate i s) _ _ r = undefined
stmt (While t s)   _ _ r = undefined

-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
