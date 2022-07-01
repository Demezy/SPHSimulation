module UsefulFunctions where

import Graphics.Gloss
import Objects

--Four elements in a tuple-----------------------------------------------------

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

--Functions for working with vectors-------------------------------------------

-- | Vector by 2 points (p1 to p2).
vectorDiff :: Point -> Point -> Vector
vectorDiff (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

-- | Vector length.
vectorMagnitude :: Vector -> Float
vectorMagnitude (x, y) = sqrt (x**2 + y**2)

-- | Distance between 2 points.
distance :: Point -> Point -> Float
distance points = vectorMagnitude.vectorDiff points

-- | Multiply vector by scalar.
vectorMul :: Vector -> Float -> Vector
vectorMul (x, y) a = (x * a, y * a)

-- | Adjust vector to some length.
normalizeVector :: Vector -> Float -> Vector
normalizeVector (x, y) l = (x * k, y * k)
  where
    k = l / vectorMagnitude (x, y)

-- | Sum of vectors.
vectorSum :: [Vector] -> Vector
vectorSum = foldr (\(a, b) (c, d) -> (a + c, b + d)) (0,0)

-- | Closest point on the segment to a given point
segmentNearestPoint :: Point -> (Point, Point) -> Point
segmentNearestPoint (e0, e1) ((a0, a1), (b0, b1))
  | ab_be > 0 = (b0, b1)
  | ab_ae < 0 = (a0, a1)
  | otherwise = (a0 + dir0 * dotP, a1 + dir1 * dotP)
    where
      (ab0, ab1) = vectorDiff (a0, a1) (b0, b1)
      (be0, be1) = vectorDiff (b0, b1) (e0, e1)
      (ae0, ae1) = vectorDiff (a0, a1) (e0, e1)
      
      ab_be = ab0 * be0 + ab1 * be1
      ab_ae = ab0 * ae0 + ab1 * ae1
      
      (dir0, dir1) = normalizeVector (ab0, ab1) 1
      dotP = dir0 * ae0 + dir1 * ae1

-- | Distance between segment and point
pointSegmentDistance :: Point -> (Point, Point) -> Float
pointSegmentDistance p w = distance p (segmentNearestPoint p w)

-- | Changing speed of time.
changeTimeMul :: Float -> Universe -> Universe
changeTimeMul x universe = universe{environment = newEnv}
  where
    env = environment universe
    prevTimeMultiplier = timeMultiplier env
    newEnv = env{timeMultiplier = prevTimeMultiplier + x}
    -- new_e (Environment t gDir gAcc d) = Environment (max (t + x) 1) gDir gAcc d

