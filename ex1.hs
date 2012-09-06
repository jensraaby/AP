type Pos = (Int, Int)
data Direction = North | South | East | West

move :: Direction -> Pos -> Pos
move North (x,y) = (x, y+1)
move West  (x,y) = (x-1, y)
move East  (x,y) = (x+1, y)
move South (x,y) = (x, y-1)

moves :: [Direction] -> Pos -> Pos 
moves [] (x,y) = (x,y)
moves (dir:ms) (x,y) = moves ms (move dir (x,y))

data Nat = Zero | Succ Nat
     deriving (Eq, Show, Read, Ord) 

--multiplyNat :: Nat -> Nat -> Nat
--multiplyNat Zero _ = Zero
--multiplyNat _ Zero = Zero
--multiplyNat x (Succ Zero)    = x
--multiplyNat x y	   = x + multiplyNat x (y-1)

-- Do these
--nat2int
--int2nat

data Tree = Leaf | Node Int Tree Tree
--insert :: Int -> Tree -> Tree
-- takes integer n and a binary search tree to which n is inserted

-- insert with polymorphism

-- extend with mult, div and sub
data Expr = Con Int
          | Add Expr Expr
     deriving (Eq, Show, Read, Ord) 

value :: Expr -> Int
value (Con n) = n
value (Add x y) = value x + value y 

-- write a pretty print function for Expr inserting few parens