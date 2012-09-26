import qualified Text.ParserCombinators.ReadP as PC

----get :: ReadP Char
--look :: ReadP String
--(+++) :: ReadP a -> ReadP a -> ReadP aSource
--(choice)
-- (<++) :: ReadP a -> ReadP a -> ReadP a
-- left choice
--gather :: ReadP a -> ReadP (String, a)
-- transforms parser into one that also returns characters read

{-
data Tree = Branch Tree Tree | Leaf deriving Show

leaf = do PC.char 'o'
          return Leaf

brackets p = do PC.char '('
                r <- p
                PC.char ')'
                return r   
               
branch = do a <- leaf PC.+++ brackets tree
            PC.char '&'
            b <- tree
            return (Branch a b)
            

    
tree = leaf PC.+++ branch PC.+++ brackets tree
-}
{-
-- make string based leaves
data Tree = Branch Tree Tree | Leaf String deriving Show

leaf = do s <- PC.many1 (PC.choice (map PC.char ['a'..'z']))
          return (Leaf s)

brackets p = do PC.char '('
                r <- p
                PC.char ')'
                return r   
               
branch = do a <- leaf PC.+++ brackets tree
            PC.char '&'
            b <- tree
            return (Branch a b)
            

    
tree = leaf PC.+++ branch PC.+++ brackets tree 

-}
-- Add operators and & or
data Operator = And | Or deriving Show
data Tree = Branch Operator Tree Tree | Leaf String deriving Show

leaf = do s <- PC.many1 (PC.choice (map PC.char ['a'..'z']))
          return (Leaf s)

brackets p = do PC.char '('
                r <- p
                PC.char ')'
                return r   
               
andBranch = do a <- leaf PC.+++ brackets tree
               PC.char '&'
               b <- andTree
               return (Branch And a b)

andTree = leaf PC.+++ brackets tree PC.+++ andBranch
        
orBranch = do a <- andTree PC.+++ brackets tree
              PC.char '|'
              b <- orTree
              return (Branch Or a b)

orTree = andTree PC.+++ orBranch

tree = orTree