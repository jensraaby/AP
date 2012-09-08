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
	deriving (Show)
insert :: Int -> Tree -> Tree
-- takes integer n and a binary search tree to which n is inserted
insert n Leaf = Node n Leaf Leaf
insert n (Node y left right) 	| n <  y = Node y (insert n left) right
								| n >= y = Node y left (insert n right)

data PolyTree a = PLeaf | PNode a (PolyTree a) (PolyTree a)
	deriving (Show)
insertPoly :: Ord a => a -> PolyTree a -> PolyTree a
insertPoly x PLeaf = PNode x PLeaf PLeaf
insertPoly x (PNode y left right) 	| x <  y = PNode y (insertPoly x left) right
									| x >= y = PNode y left (insertPoly x right)
-- insert with polymorphism

-- extend with mult, div and sub
data Expr = Con Int
          | Add Expr Expr
		  | Sub Expr Expr
		  | Mul Expr Expr
		  | Div Expr Expr
     deriving (Eq, Read, Ord) 

value :: Expr -> Int
value (Con n) = n
value (Add x y) = value x + value y
value (Sub x y) = value x - value y
value (Mul x y) = value x * value y
value (Div x y) = (value x) `div` (value y)

--Pretty printing
--http://learnyouahaskell.com/making-our-own-types-and-typeclasses#derived-instances
instance Show Expr where
	show (Con n)   = show n
	show (Add x y) = show x ++ " + " ++ show y
	show (Sub x y) = show x ++ " - " ++ show y
	show (Mul x y) = show x ++ " * " ++ show y
	show (Div x y) = show x ++ " / " ++ show y


-- Morse
--data Morse = Empty | Dot Morse | Dash Morse deriving (Show
encodings :: [([Char],[Char])]
encodings = [("A",".-")
			,("B","-...")
			,("C","-.-.")
			,("D","-..")
			,("E",".")
			,("F","..-.")
			,("G","--.")
			,("H","....")
			,("I","..")
			,("J",".---")
			,("K","-.-")
			,("L",".-..")
			,("M","--")
			,("N","-.")
			,("O","---")
			,("P",".--.")
			,("Q","--.-")
			,("R",".-.")
			,("S","...")
			,("T","-")
			,("U","..-")
			,("V","...-")
			,("W",".--")
			,("X","-..-")
			,("Y","-.--")
			,("Z","--..")]
encode :: String -> String
encode "" = ""
encode (c:cs) = morse 
	where morse = lookup c encodings
