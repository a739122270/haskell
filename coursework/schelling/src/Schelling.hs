module Schelling
  ( Coord
  , AgentType (..)
  , Cell

  , step
  ) where

import System.Random

-- type-definition of a coordinate in our world
type Coord = (Int, Int)

-- data definition for the agent types
data AgentType 
  = Red       -- ^ Red agent
  | Green     -- ^ Green agent
  | Blue      -- ^ Blue agent
  deriving (Eq, Show) -- Needed to compare for equality, otherwise would need to implement by ourself

-- Type-definition of a Cell: it is just a coordinate and an optional AgentType.
-- Note: the agent type is optional and is Nothing in case the cell is empty.
type Cell = (Coord, Maybe AgentType)

-- Computes one step of the Schelling Segregation model.
step :: [Cell]           -- ^ All cells of the world
     -> StdGen           -- ^ The random-number generator
     -> Double           -- ^ The ratio of equal neighbours an agent requires to be happy
     -> ([Cell], StdGen) -- ^ Result is the new list of cells and the updated random-number generator
step cs g ratio = (cs', g')
  where
    -- filter all empty cells
    csEmpty = filter isEmpty cs

    -- filter all agents
    csNonEmpty = filter (not . isEmpty) cs
    -- filter all unhappy agents
    csUnhappy = filter (not . isHappy ratio cs) csNonEmpty
    -- filter all happy agents
    csHappy = filter (isHappy ratio cs) csNonEmpty
    
    -- Move the unhappy agents to a random empty cell. Note we use fold(r) for 
    -- this because we need to update data while iterating over the unhappy
    -- agents: cells can become empty or occupied.
    (csUnhappy', csEmpty', g') = foldr moveCell ([], csEmpty, g) csUnhappy 

    -- create a new list of cells for the result of this step:
    -- the happy agents (which have not moved) plus the updated unhappy agents
    -- plus the updated empty cells
    cs' = csHappy ++ csUnhappy' ++ csEmpty'

    -- Returns True if a cell is empty
    isEmpty :: Cell -> Bool
    isEmpty (_, Nothing) = True
    isEmpty _            = False

-- Returns True if an agent on a given cell is happy or not
isHappy :: Double  -- ^ The satisfaction factor
        -> [Cell]  -- ^ All cells
        -> Cell    -- ^ The cell with the agent
        -> Bool    -- ^ True in case the agent is happy, False otherwise
isHappy ratio cs c = True 
  -- where
    -- TODO implement
    -- 1. compute neighbour coordinates
    -- 2. get all cells for the neighbour coordinates
    -- 3. compute the ratio (include empty cells!)

-- Moves an unhappy agent to an empty cell.
-- The moved agent must be appended to the first list. the new empty cell must
-- be appended to the second list in the return tuple. The StdGen is used to
-- draw random numbers.
moveCell :: Cell                      -- ^ The unhappy agent to move
         -> ([Cell], [Cell], StdGen)  -- ^ The already move agents, the empty cells, the random-number generator
         -> ([Cell], [Cell], StdGen)  -- ^ The already move agents, the empty cells, the random-number generator
moveCell c (csm, cse, g) = (csm, cse, g)
  -- where
    -- TODO: implement
    -- 1. get a random empty cell (cse contains empty only)

    -- 2. swap the contents of the two cells:
    -- empty cell becomes occupied by agent
    -- occupied cell becomes empty

    -- 3. add newly occupied cell to the accumulator of occupied cells 

    -- 4. remove the previously empty cell, which is occupied now, from the list
    -- of empty cells

