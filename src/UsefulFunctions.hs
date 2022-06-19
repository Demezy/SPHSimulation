module UsefulFunctions where

import Graphics.Gloss 

-- | First element in four elements tuple. 
firstOfTuple :: (a,b,c,d) -> a
firstOfTuple (x, _, _, _) = x

-- | Second element in four elements tuple. 
secondOfTuple :: (a,b,c,d) -> b
secondOfTuple (_, x, _, _) = x

-- | Third element in four elements tuple. 
thirdOfTuple :: (a,b,c,d) -> c
thirdOfTuple (_, _, x, _) = x

-- | Fourth element in four elements tuple. 
fourthOfTuple :: (a,b,c,d) -> d
fourthOfTuple (_, _, _, x) = x
