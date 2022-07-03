module Metaballs where

import Data.List
import Graphics.Gloss
import Objects
import QuadTree
import Cluster
import UsefulFunctions

--------------------------------------------------------------------------------

type Shape = Point -> Float

--------------------------------------------------------------------------------

circlee :: Point -> Float -> Shape
circlee (x0, y0) r (x, y) = sqrt ((x0 - x)^2 + (y0 - y)^2) - r

(∪) :: Shape -> Shape -> Shape
a ∪ b = \p -> min (a p) (b p)

shapes :: [Particle] -> Shape
shapes = collecting

collecting :: [Particle] -> Shape
collecting []       = circlee (0,0) 0
collecting [x]      = circlee (dx, dy) 2
    where
        coordinate = position x
        dx = fst coordinate
        dy = snd coordinate
collecting (x : xs) = circlee (dx, dy) 2 ∪ collecting xs
    where
        coordinate = position x
        dx = fst coordinate
        dy = snd coordinate

--------------------------------------------------------------------------------
listOfVectors :: [[Particle]] -> [Picture]
listOfVectors particles = map (\p -> polygon (sortOn (distance (0,0)) (map position p))) particles

vectorsToPicture :: [Particle] -> [Picture]
vectorsToPicture particles = listOfVectors (dbscan (getParticleTree particles) 1 0 (map NotVisited particles))
--------------------------------------------------------------------------------