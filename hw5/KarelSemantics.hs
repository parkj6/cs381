--Team: Cory Hayes (hayescor), Jong Park (parkj6), Jacob Lyons (lyonsja)

module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)

import KarelSyntax
import KarelState


-- | Valuation function for Test.
test :: Test -> World -> Robot -> Bool
test (Not t)    w r        = not (test t w r) 
test (Facing c) _ r        = c == (getFacing r)
test (Clear d)  w r        = isClear (relativePos d r) w
test Beeper     w r        = hasBeeper (getPos r) w
test Empty      _ r        = isEmpty r

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

-- | Testing 'Test' code
demoWorld :: World
demoWorld (4,1) = Just 2
demoWorld (6,1) = Just 3
demoWorld (8,1) = Just 4
demoWorld (x,y) | x >= 0 && x < 10 &&
                  y >= 0 && y < 5      = Just 0
                | otherwise            = Nothing
				
demoBot :: Robot
demoBot = ((4,1),East,1)