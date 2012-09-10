module World
( validMove
, fromList
) where

data Direction = North | South | West | East 
               deriving (Show,Eq,Read,Enum)
type Position = (Int, Int)