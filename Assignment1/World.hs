{-
Advanced Programming - Assignment 1
Jens Raaby & Erik Partridge
September 2012
-}
module World
( fromList
, validMove
, Direction(..)
)
where

{-
Import some very helpful modules
-}
import qualified Data.Map as Map
import Data.Maybe
import Test.HUnit

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
		walls1     <- walls m p1
		walls2     <- walls m p2
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

-- Get the list of walls for the given position
walls :: Maze -> Position -> Maybe Cell
walls m p0 = Map.lookup p0 (cells m)

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

-- runProg :: Maze -> Program -> ([Position], Direction)
-- runProg maze program = do
--         w <- (initializeWorld maze)
--         put $ robot w -- doesn't work
--         result <- interp program
--         hist   <- (history $ robot result) ++ currentPos $ robot result
--         dir    <- currentDir $ robot result
--         (hist,dir)
        
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

-- Moving will reposition the robot one step forwards in its current direction
--  or it will fail if this is not possible
moving :: RobotCommand ()
moving = do
    w <- get
    pos <- return $ newPos (currentDir (robot w)) (currentPos $ robot w)
    case (validMove (maze w) (currentPos $ robot w) pos ) of
        True -> put $ (robot w){currentPos = pos}
        False -> fail "Robot cannot move from currentpos to pos while facing dir"


{- 
-- Function to evaluate a condition in the current state (returning RobotCommand Bool)
-}
evalCond :: Cond -> RobotCommand Bool
evalCond (Wall rel) =
    do
        w <- get
        currentWalls <- return( fromMaybe [] $ walls (maze w) $ currentPos (robot w))
        dir <- return (currentDir (robot w))
        wall <- case rel of 
            Ahead   -> return $ dir `elem` currentWalls
            ToLeft  -> return $ (turnhelp Counterclockwise dir) `elem` currentWalls
            ToRight -> return $ (turnhelp Clockwise dir) `elem` currentWalls
            Behind  -> return $ (turnhelp Clockwise $ turnhelp Clockwise dir) `elem` currentWalls
        return wall
        
evalCond (And c1 c2) = 
    do
        a <- evalCond c1
        b <- evalCond c2
        return (and [a,b])

evalCond (Not c) = 
    do
        this <- evalCond c
        return (not this)
        
evalCond AtGoalPos = 
    do 
        w <- get 
        return (currentPos (robot w) == (cols (maze w),rows (maze w)))
 
{-
-- interp takes a statment and returns a RobotCommand (i.e. a changed state)
-}
interp :: Stm -> RobotCommand ()
interp Forward = do
    moving
    return ()
    
interp Backward = do
    turning Clockwise
    turning Clockwise
    moving
    return ()

interp TurnRight = do
    turning Clockwise
    return ()
    
interp TurnLeft = do 
    turning Counterclockwise
    return ()
    
interp (If cond s1 s2) = 
    do
        c <- evalCond cond
        case c of
            True  -> interp s1
            False -> interp s2        
             
interp (While cond s)  = 
    do
        c <- evalCond cond
        case c of
            True  -> interp (Block [s, While cond s])
            False -> return ()
        
interp (Block ([]))    = return ()
interp (Block (x:xs))  = 
    do
        interp x
        interp (Block xs)
        return ()

--------------------------------------------------
-- Test data                            
--------------------------------------------------
-- the Test Maze supplied in the assignment 
testMazeList = [((0,0),[North,South,West]),((0,1),[North,South,West])
                    ,((0,2),[South,West]),((0,3),[West,East])
                    ,((0,4),[North,West]),((1,0),[South]),((1,1),[North])
                    ,((1,2),[South,East]),((1,3),[North,West])
                    ,((1,4),[North,South,East]),((2,0),[North,South])
                    ,((2,1),[South,East]),((2,2),[West,East]),((2,3),[])
                    ,((2,4),[North,West,East]),((3,0),[North,South])
                    ,((3,1),[South,West]),((3,2),[West])
                    ,((3,3),[]),((3,4),[North,West,East])
                    ,((4,0),[North,South,East]),((4,1),[North,South,East])
                    ,((4,2),[North,South,East]),((4,3),[South,East])
                    ,((4,4),[North,West,East])]
test1_cols = 5
test1_rows = 5
                    
test2MazeList = [((0,0),[South,West]),((0,1),[North,West])
		   ,((1,0),[North,South,East]),((1,1),[North,South,East])]
test2_rows = 2
test2_cols = 2

maze1 = fromList(testMazeList)
maze2 = fromList(test2MazeList)

test1 = TestCase $ assertBool "Maze 1 columns" $ test1_cols == (cols maze1)
test2 = TestCase $ assertBool "Maze 1 rows" $ test1_rows == (rows maze1)
test3 = TestCase $ assertBool "Maze 2 columns" $ test2_cols == (cols maze2)
test4 = TestCase $ assertBool "Maze 2 rows" $ test2_rows == (rows maze2)
test5 = TestCase $ assertBool "Maze 2 validMove" $ validMove maze2 (0,0) (0,1)
test6 = TestCase $ assertBool "Maze 2 invalidMove" $ not $ validMove maze2 (0,0) (1,1)

basicTests = TestList [test1,test2,test3,test4,test6]

