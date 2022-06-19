module UsefulFunctions where

import Graphics.Gloss 

-- | First element in four elements tuple. 
f :: (a,b,c,d) -> a
f (x, _, _, _) = x

-- | Second element in four elements tuple. 
s :: (a,b,c,d) -> b
s (_, x, _, _) = x

-- | Third element in four elements tuple. 
t :: (a,b,c,d) -> c
t (_, _, x, _) = x