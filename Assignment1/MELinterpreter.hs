{-
Advanced Programming - Assignment 1
Jens Raaby & Erik Partridge
September 2012
-}
data Relative = Ahead | ToLeft | ToRight | Behind
              deriving (Eq, Show)

data Cond = Wall Relative
          | And  Cond Cond
          | Not  Cond
          | AtGoalPos
          deriving (Eq, Show)
            
-- Statement could be a list of statements in a block.
data Stm = Forward
         | Backward
         | TurnRight
         | TurnLeft    
         | If Cond Stm Stm
         | While Cond Stm
         | Block [Stm]
         deriving (Eq, Show)
         
 -- Type Robot
 data Robot = ... --state of robot (position in maze, direction facing, history of positions so far)
 
 -- Type World for modelling maze and robot
 data World =...
 
initialWorld :: Maze -> World
-- takes a maze and creates an initial world with robot at 0,0 facing north

newtype RobotCommand a = RC { runRC :: World -> ... }
-- make it a monad instance
-- mazes don't change during execution (reflected by type)
-- type specifies how to deal with errors - document that

interp :: Stm -> RobotCommand ()
-- computes effect of executing a robot statement

runProg :: Maze -> Program -> ([Postion], Direction)
-- runs a program started from the initial world (from given maze), returns the path of the robot and its final direction