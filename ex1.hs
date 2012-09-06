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

-- from slides:
add x Zero = x
add x (Succ n) = add (Succ x) n

multiply :: Nat -> Nat -> Nat
multiply _ Zero = Zero
multiply Zero _ = Zero
multiply x (Succ Zero) = x
multiply (Succ x) y = add y (multiply x y)

--nat2int
nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ x) = 1 + nat2int x

--int2nat
int2nat :: Int -> Nat
int2nat 0 = Zero
int2nat n = Succ (int2nat (n-1))

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