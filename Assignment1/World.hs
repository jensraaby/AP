{- 
put this back after World: 
( validMove
, fromList
) 
-} 
module World (fromList,validMove,Position,Direction)
where

import qualified Data.Map as Map
import Data.Maybe

data Direction = North | South | West | East 
               deriving (Show,Eq,Read,Enum)
type Position = (Int, Int)

-- type Cell - a list of walls for that cell (Eg east and north)
type Cell = [Direction]

-- functions for manipulating positions
-- TODO: error handling in a environment sensitive
move :: Direction -> Position -> Position
move North (x,y) = (x, y+1)
move West  (x,y) = (x-1, y)
move East  (x,y) = (x+1, y)
move South (x,y) = (x, y-1)


-- a maze is a grid of cells (M rows, N columns). cells denoted by pairs of integers, origin lower left
data Maze = Maze { rows :: Int
                 , cols :: Int
                 , cells :: Map.Map Position Cell} deriving (Show)
                 {- Haskell makes the functions 
                     rows  :: Maze -> Int
                     cols  :: Maze -> Int
                     cells :: Maze -> Map.Map Position Cell
                 -}
{- 
Here we construct a map of positions to cells 
and work out the number of rows and columns. Lovely.

The little +1 at the end of the rows and cols definition 
is there because the maze is indexed from zero
-}
fromList :: [(Position, [Direction])] -> Maze
fromList m = Maze {cells = Map.fromList m
                 , cols = (maximum $ map fst $ map fst m) +1
                 , rows = (maximum $ map snd $ map fst m) +1
                 }

validMove :: Maze -> Position -> Position -> Bool
validMove m p1 p2 = fromMaybe False $ maybeValidMove m p1 p2

maybeValidMove :: Maze -> Position -> Position -> Maybe Bool
maybeValidMove m p1 p2 = do
		walls1     <- Map.lookup p1 (cells m)
		walls2     <- Map.lookup p2 (cells m)
		border     <- moveDirection p1 p2
		inverseborder  <- moveDirection p2 p1
		Just $ not $ (elem border walls1) && (elem inverseborder walls2)


-- Helper function to determine the direction of a move
--  returns nothing if the positions are not adjacent
moveDirection :: Position -> Position -> Maybe Direction
moveDirection (x1,y1) (x2,y2)
	| x2 == x1   && y2 == y1+1 = Just North
	| x2 == x1   && y2 == y1-1 = Just South
	| x2 == x1+1 && y2 == y1   = Just East
	| x2 == x1-1 && y2 == y1   = Just West
	| otherwise = Nothing

