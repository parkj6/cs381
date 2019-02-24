-- | This module defines the state of a running Karel program.
module KarelState where

import Prelude hiding (Either(..))
import KarelSyntax


--
-- * Positions and directions
--

-- | A cartesian coordinate, representing a position in the world.
type Pos = (Int,Int)

-- | Get the position next to the given position, offset by one square
--   in the indicated *cardinal* direction.
neighbor :: Card -> Pos -> Pos
neighbor North (x,y) = (x,y+1)
neighbor South (x,y) = (x,y-1)
neighbor East  (x,y) = (x+1,y)
neighbor West  (x,y) = (x-1,y)

-- | Get a cardinal direction relative to the current one.
cardTurn :: Dir -> Card -> Card
cardTurn Front c     = c
cardTurn Back  North = South
cardTurn Back  South = North
cardTurn Back  East  = West
cardTurn Back  West  = East
cardTurn Left  North = West
cardTurn Left  South = East
cardTurn Left  East  = North
cardTurn Left  West  = South
cardTurn Right North = East
cardTurn Right South = West
cardTurn Right East  = South
cardTurn Right West  = North


--
-- * World state
--

-- | The state of the world is represented by a function that returns
--   for each position:
--    * Nothing, if the position is a wall
--    * Just k, if the position is clear and has k beepers
--   You can assume k is always >= 0.
type World = Pos -> Maybe Int

-- | Is the given position clear?
isClear :: Pos -> World -> Bool
isClear p w = w p /= Nothing

-- | Is there a beeper at the given position?
hasBeeper :: Pos -> World -> Bool
hasBeeper p w = maybe False (>0) (w p)

-- | Increment the number of beepers at the given position.
incBeeper :: Pos -> World -> World
incBeeper p w = \q -> if p == q 
                         then case w q of
                                Nothing -> Just 1
                                Just i  -> Just (i+1)
                         else w q

-- | Decrement the number of beepers at the given position. Note that this
--   function can yield a world with negative beepers, so you should make
--   sure to only decrement the beepers at a position after first checking
--   to make sure there is at least one beeper there (using `hasBeeper`).
decBeeper :: Pos -> World -> World
decBeeper p w = \q -> if p == q then fmap (subtract 1) (w q) else w q


--
-- * Robot state
--

-- | The state of the robot is represented by a triple containing:
--    * the current position
--    * the current facing (cardinal direction)
--    * the number of beepers in the beeper bag
type Robot = (Pos,Card,Int)


-- ** Robot position

-- | The robot's position.
getPos :: Robot -> Pos
getPos (p,_,_) = p

-- | Get a position relative to the robot's current facing and position.
relativePos :: Dir -> Robot -> Pos
relativePos d (p,c,_) = neighbor (cardTurn d c) p

-- | Set the robot's position.
setPos :: Pos -> Robot -> Robot
setPos p (_,c,b) = (p,c,b)

-- | Update the robot's position using an update function.
updatePos :: (Pos -> Pos) -> Robot -> Robot
updatePos f (p,c,b) = (f p,c,b)


-- ** Robot facing

-- | The robot's facing (cardinal direction).
getFacing :: Robot -> Card
getFacing (_,c,_) = c

-- | Set the robot's facing.
setFacing :: Card -> Robot -> Robot
setFacing c (p,_,b) = (p,c,b)

-- | Update the robot's facing using an update function.
updateFacing :: (Card -> Card) -> Robot -> Robot
updateFacing f (p,c,b) = (p,f c,b)


-- ** Beeper bag

-- | The number of beepers in the beeper bag.
getBag :: Robot -> Int
getBag (_,_,b) = b

-- | Is the beeper bag empty?
isEmpty :: Robot -> Bool
isEmpty (_,_,b) = b <= 0

-- | Increment the number of beepers in the bag.
incBag :: Robot -> Robot
incBag (p,c,b) = (p,c,b+1)

-- | Decrement the number of beepers in the bag.
decBag :: Robot -> Robot
decBag (p,c,b) = (p,c,b-1)


--
-- * Statement results
--

-- | The result of executing a statement.
-- 
--     * OK: The statement executed successfully, so return the updated state
--       of the world and the robot. OK to execute the next statement.
--
--     * Done: Produced only by the Shutdown statement. This returns the final
--       state of the robot. No further statements should be executed.
--
--     * Error: An error occurred. Includes a string for reporting error messages.
--       No further statements should be executed.
--
data Result = OK    World Robot
            | Done  Robot
            | Error String

instance Show Result where
  show (OK _ r)  = "OK: " ++ show r
  show (Done r)  = "Done: " ++ show r
  show (Error s) = "Error: " ++ s

-- | Applies a function to the result if its an OK, otherwise returns
--   the result unchanged.
onOK :: (World -> Robot -> Result) -> Result -> Result
onOK f (OK w r) = f w r
onOK _ result   = result
