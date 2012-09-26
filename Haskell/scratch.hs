-- creating new types in Haskell
type Pos = (Int,Int) -- just a synonym
newtype Meters = M Int deriving (Show) -- 
newtype Seconds = S Int deriving (Show)
newtype Speed = SP Int deriving (Show)

speed :: Meters -> Seconds -> Speed
speed (M m) (S s) = SP (m `div` s)