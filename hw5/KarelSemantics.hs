--Team: Cory Hayes (hayescor), Jong Park (parkj6), Jacob Lyons (lyonsja)

module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)

import KarelSyntax
import KarelState


-- | Valuation function for Test.
test :: Test -> World -> Robot -> Bool
test (Not t)    w r = not (test t w r) 
test (Facing c) _ r = undefined
test (Clear d)  w (p,c,b) = isClear p w
test Beeper     _ r = undefined
test Empty      _ r = undefined

-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown      _ _ r = Done r
stmt PickBeeper    _ w r = let p = getPos r
                        in if hasBeeper p w
                              then OK (decBeeper p w) (incBag r)
                              else Error ("No beeper to pick at: " ++ show p)
stmt Move          _ _ r = undefined
stmt PutBeeper     _ _ r = undefined
stmt (Turn d)      _ _ r = undefined
stmt (Call m)      _ _ r = undefined
stmt (Iterate i s) _ _ r = undefined
stmt (If t s1 s2)  _ _ r = undefined
stmt (While t s)   _ _ r = undefined
stmt (Block (a:b)) _ _ r = undefined

-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
