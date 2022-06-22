module UsefulFunctions where

import Graphics.Gloss
import Objects

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

-- | Vector by 2 points (p1 to p2)
vectorDiff :: Point -> Point -> Vector
vectorDiff (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

-- | Vector length
vectorMagnitude :: Vector -> Float
vectorMagnitude (x, y) = sqrt (x**2 + y**2)

-- | Distance between 2 points
distance :: Point -> Point -> Float
distance points = vectorMagnitude.vectorDiff points

-- | Multiply vector by scalar
vectorMul :: Vector -> Float -> Vector
vectorMul (x, y) a = (x * a, y * a)

-- | Adjust vector to some length
normalizeVector :: Vector -> Float -> Vector
normalizeVector (x, y) l = (x * k, y * k)
  where
    k = l / vectorMagnitude (x, y)

-- | Sum of vectors
vectorSum :: [Vector] -> Vector
vectorSum = foldr1 (\(a, b) (c, d) -> (a + c, b + d))
