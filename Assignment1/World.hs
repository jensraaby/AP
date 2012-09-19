{- 
put this back after World: 
( validMove
, fromList
) 
-} 
module World (fromList,Position,Direction)
where

data Direction = North | South | West | East 
               deriving (Show,Eq,Read,Enum)
type Position = (Int, Int)

-- type Cell - a list of walls for that cell (Eg east and north)
type Cell = [Direction]

-- functions for manipulating positions
move :: Direction -> Position -> Position
move North (x,y) = (x, y+1)
move West  (x,y) = (x-1, y)
move East  (x,y) = (x+1, y)
move South (x,y) = (x, y-1)


-- a maze is a grid of cells (M rows, N columns). cells denoted by pairs of integers, origin lower left
type Maze = [[Cell]]

sizeMaze :: Maze -> (Int,Int)
sizeMaze = undefined
-- functions for querying the maze in various ways

--validMove :: Maze -> Position -> Position -> Bool
--validMove = undefined

fromList :: [(Position, [Direction])] -> Maze
fromList = undefined

