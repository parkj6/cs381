module KarelSemantics where

import Prelude hiding (Either(..))
import Data.Function (fix)

import KarelSyntax
import KarelState


-- | Valuation function for Test.
test :: Test -> World -> Robot -> Bool
test (Not t)    w r = not (test t w r)
test (Facing c) _ r = getFacing r == c
test (Clear d)  w r = isClear (relativePos d r) w
test Beeper     w r = hasBeeper (getPos r) w
test Empty      _ r = getBag r == 0

-- | Valuation function for Stmt.
stmt :: Stmt -> Defs -> World -> Robot -> Result
stmt Shutdown      _ _ r = Done r
stmt Move          _ w r = let p = relativePos Front r
                           in if isClear p w
                              then OK w (setPos p r)
                              else Error ("Blocked at: " ++ show p)
stmt PickBeeper    _ w r = let p = getPos r
                           in if hasBeeper p w
                              then OK (decBeeper p w) (incBag r)
                              else Error ("No beeper to pick at: " ++ show p)
stmt PutBeeper     _ w r = let p = getPos r
                           in if isEmpty r
                              then Error "No beeper to put."
                              else OK (incBeeper p w) (decBag r)
stmt (Turn d)      _ w r = OK w $ updateFacing (cardTurn d) r
stmt (Call f)      m w r = case lookup f m of
                             Just s  -> stmt s m w r
                             Nothing -> Error ("Undefined macro: " ++ f)
stmt (Iterate i s) m w r = stmts (replicate i s) m w r
stmt (If t s1 s2)  m w r = stmt (if test t w r then s1 else s2) m w r
stmt (While t s)   m w r = if test t w r
                           then onOK (stmt (While t s) m) (stmt s m w r)
                           else OK w r
stmt (Block ss)    m w r = stmts ss m w r
    
-- | Evaluate a sequence of statements.
stmts :: [Stmt] -> Defs -> World -> Robot -> Result
stmts []     _ w r = OK w r
stmts (s:ss) m w r = onOK (stmts ss m) (stmt s m w r)

-- | Run a Karel program.
prog :: Prog -> World -> Robot -> Result
prog (m,s) w r = stmt s m w r
