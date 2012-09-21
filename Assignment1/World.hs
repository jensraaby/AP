{-
Advanced Programming - Assignment 1
Jens Raaby & Erik Partridge
September 2012
-}
module World
where

{-
Import some very helpful modules
-}
import qualified Data.Map as Map
import Data.Maybe

-----------------------------------------------------------------
-- Types
-----------------------------------------------------------------
data Direction = North | South | West | East 
               deriving (Show,Eq,Read,Enum)
type Position = (Int, Int)

-- type Cell - a list of walls for that cell (Eg [East,North])
type Cell = [Direction]

-- a maze is a grid of cells (M rows, N columns). cells denoted by pairs of integers, origin 0,0
data Maze = Maze { rows :: Int
                 , cols :: Int
                 , cells :: Map.Map Position Cell} deriving (Show)
                 {- Haskell makes the functions 
                     rows  :: Maze -> Int
                     cols  :: Maze -> Int
                     cells :: Maze -> Map.Map Position Cell
                 -}

-- Robot state:
data Robot = Robot { currentPos :: Position
                   , currentDir :: Direction
                   , history    :: [Position]
                   } deriving (Show)
                   
-- World State:
data World = World { maze :: Maze
                   , robot :: Robot    
                   }
                   
                   
                   
-----------------------------------------------------------------
-- Function definitions part 1
-----------------------------------------------------------------
initializeWorld :: Maze -> World
initializeWorld m = World m r
			where r = Robot (0,0) North []
   
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

{- 
validMove - takes a move and 2 positions p1 and p2
  returns true if the move from p1 to p2 is possible
-}
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

-- Get a new position given direction and previous position
newPos :: Direction -> Position -> Position
newPos North (x,y) = (x, y+1)
newPos West  (x,y) = (x-1, y)
newPos East  (x,y) = (x+1, y)
newPos South (x,y) = (x, y-1)

-- Move the robot in given world and direction
move :: World -> Direction -> Either String Robot
move w d = if d == oldDir then 
                if (validMove (maze w) oldP pos) then Right $ Robot pos d hist
                    else Left $ "There is a wall between " ++ show(oldP) ++ " and " ++ show(pos)
           else Left $ "Robot cannot move from " ++ show(oldP) ++ " in direction " ++ show(d) ++ " because it is facing " ++ show(oldDir)
           where
            r = robot w
            oldHist = history r
            oldP = currentPos r
            oldDir = currentDir r
            pos = newPos d oldP
            hist = oldHist ++ [oldP]
        

-----------------------------------------------------------------
-- Abstract Syntax 
-----------------------------------------------------------------

data Relative = Ahead | ToLeft | ToRight | Behind
              deriving (Eq, Show)

data Cond = Wall Relative
          | And  Cond Cond
          | Not  Cond
          | AtGoalPos
          deriving (Eq, Show)

evalCond :: Cond -> Bool
evalCond _ = False
-- Needs to be filled in for each case!
            
-- Statement could be a list of statements in a block.
data Stm = Forward
         | Backward
         | TurnRight
         | TurnLeft    
         | If Cond Stm Stm
         | While Cond Stm
         | Block [Stm]
         deriving (Eq, Show)

-- Programs are just statements
type Program = Stm

-----------------------------------------------------------------
-- Definition of the computations
-----------------------------------------------------------------
{- 
RobotCommand is a computation

The World state comprises a maze and a robot. As only the robot is 
affected by a computation, the type here returns the result (type a) 
and the altered robot state.

In case of an error, the computation returns an error string.
Hence the 'Either' in the type declaration

-}
data Rotation = Clockwise | Counterclockwise
			deriving(Show,Eq,Read,Enum)

newtype RobotCommand a = RC { runRC :: World -> Either String (a,Robot) }

-- Instantiate the RobotCommand as a Monad:
instance Monad RobotCommand where
    -- return :: a -> RobotCommand a
	return x = RC $ \world -> Right (x, robot world)
     -- (>>=) :: RobotCommand a -> (a -> RobotCommand b) -> RobotCommand b
	(RC h) >>= f = RC $ \world -> case h world of
                    Left err            -> do fail err
                    Right (robot,newRC) -> let 
                                              RC g = f robot
                                              newWorld = World (maze world) newRC
                                              in g newWorld                    
-- 	fail err = RC $ \world -> Left err

-- Get - returns the current state
get :: RobotCommand World                            
get = RC $ \world -> Right (world,robot world)

-- Put - updates the state
put :: Robot -> RobotCommand ()
put robot = RC ( \world -> Right ((),robot))


runProg :: Maze -> Program -> ([Position], Direction)
runProg maze program = undefined
-- runs a program started from the initial world (from given maze), returns the path of the robot and its final direction

turnhelp :: Rotation -> Direction -> Direction
turnhelp Clockwise North = East
turnhelp Clockwise East = South
turnhelp Clockwise South = West
turnhelp Clockwise West = North
turnhelp Counterclockwise North = West
turnhelp Counterclockwise West = South
turnhelp Counterclockwise South = East
turnhelp Counterclockwise East = North
-- turnhelp is a simple exhaustive helper function for turning

turning :: Rotation -> RobotCommand ()
turning rotate = do
	w <- get
	put $ (robot w){currentDir = (turnhelp rotate (currentDir (robot w)))}
-- the turning function takes a rotation and gets the running world, returning a robot with updated facing.

moving :: RobotCommand ()
moving = undefined
{- moving = do
	w <- get
	case s of
	where s = (move w (currentDir (robot w)))
	 --if Left string, fail
	 --if robot, then put the below line
	--put $ (move w (currentDir (robot w)))
-}



interp :: Stm -> RobotCommand ()
interp Forward         = do
                            moving
			                return ()
interp Backward        = do
                            turning Clockwise
			                turning Clockwise
			                moving
                            return ()
interp TurnRight       = do
			                turning Clockwise
			                return () 
interp TurnLeft        = do 
            			    turning Counterclockwise
            			    return ()
interp (If cond s1 s2) = do
            			    case (evalCond cond) of
            			      True  -> interp s1
            			      False -> interp s2			
interp (While cond s)  = do
            			    case (evalCond cond) of
            			      True  -> interp (Block [s, While cond s])
            			      False -> return ()
interp (Block ([]))    =    return ()
interp (Block (x:xs))  = do
            			    interp x
            			    interp (Block xs)
            			    return ()

simMaze = fromList [((0,0),[South,West]),((0,1),[North,West])
		   ,((1,0),[North,South,East]),((1,1),[North,South,East])]

-- algorithm to generate a maze, then test that you can navigate
