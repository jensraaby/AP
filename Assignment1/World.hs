{- 
put this back after World: 
( validMove
, fromList
) 
-} 
module World (fromList,Position,Direction)
where

import qualified Data.Map as Map

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
data Maze = Maze { rows :: Int
                 , cols :: Int
                 , cells :: Map.Map Position Cell} deriving (Show)
                 {- Haskell makes the functions 
                     rows  :: Maze -> Int
                     cols  :: Maze -> Int
                     cells :: Maze -> Map.Map Position Cell
                 -}
-- Want to be able to check whether it is possible to pass between two cells with
opposingDirections :: [(Direction,Direction)]
opposingDirections = [(West,East),(East,West),(North,South),(South,North)]

checkMove :: (Position,Maybe Cell) -> (Position,Maybe Cell) -> Bool
checkMove (_,Nothing) _ = False
checkmove _ (_,Nothing) = False
checkMove (p1,Just walls1) (p2,Just walls2) 
    | p1 == move North p2 = (not (elem South walls1) && not (elem North walls2))
    | p1 == move South p2 = (not (elem North walls1) && not (elem South walls2))
    | p1 == move East  p2 = (not (elem West  walls1) && not (elem East  walls2))
    | p1 == move West  p2 = (not (elem East  walls1) && not (elem West  walls2))
    | otherwise = False

validMove :: Maze -> Position -> Position -> Bool
-- Could need to add error handling on invalid position
validMove m p1 p2 = checkMove (p1,walls1) (p2,walls2)
                where walls1 = Map.lookup p1 (cells m)
                      walls2 = Map.lookup p2 (cells m)
                      
{- 
Here we construct a map of positions to cells 
and work out the number of rows and columns. Lovely.

The little +1 at the end of the rows and cols definition 
is there because the maze is indexed from zero
-}
fromList :: [(Position, [Direction])] -> Maze
fromList m = Maze {cells = Map.fromList m
                 , rows = (maximum $ map fst $ map fst m) +1
                 , cols = (maximum $ map snd $ map fst m) +1
                 }

