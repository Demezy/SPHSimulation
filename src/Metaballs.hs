module Metaballs where

import Data.List
import Data.Bifunctor
import Data.Foldable
import Prelude hiding (Left, Right)
import Graphics.Gloss
import Objects
import QuadTree
import Cluster
import UsefulFunctions

--------------------------------------------------------------------------------

data Tree_ a = Root (Tree_ a) (Tree_ a) (Tree_ a) (Tree_ a) |
               Empty | Full | Leaff a
     deriving (Show, Eq)

data Side = Upper | Lower | Left | Right
  deriving Show

type Shape = Point -> Float
type Min = Point
type Max = Point
type Cell = (Min, Max)
type Tree = Tree_ Cell
type Edge = (Point, Point)

--------------------------------------------------------------------------------

instance Foldable Tree_ where
    foldMap f (Leaff a) = f a
    foldMap f (Root a b c d) = mconcat $ map (foldMap f) [a, b, c, d]
    foldMap _ _ = mempty

--------------------------------------------------------------------------------

circlee :: Point -> Float -> Shape
circlee (x0, y0) r (x, y) = sqrt ((x0 - x)^2 + (y0 - y)^2) - r

(∪) :: Shape -> Shape -> Shape
a ∪ b = \p -> min (a p) (b p)

shapes :: [Particle] -> Shape
shapes = collecting

collecting :: [Particle] -> Shape
collecting []       = circlee (0,0) 0
collecting [x]      = circlee (dx, dy) 10
    where
        coordinate = position x
        dx = fst coordinate
        dy = snd coordinate
collecting (x : xs) = circlee (dx, dy) 10 ∪ collecting xs
    where
        coordinate = position x
        dx = fst coordinate
        dy = snd coordinate

--------------------------------------------------------------------------------

buildTree :: Min -> Max -> Int -> Tree
buildTree min max 0 = Leaff (min, max)
buildTree (xmin, ymin) (xmax, ymax) i =
    Root (buildTree (xmin, ymin) (xmid, ymid) (i - 1))
         (buildTree (xmid, ymin) (xmax, ymid) (i - 1))
         (buildTree (xmin, ymid) (xmid, ymax) (i - 1))
         (buildTree (xmid, ymid) (xmax, ymax) (i - 1))
    where xmid = (xmin + xmax) / 2
          ymid = (ymin + ymax) / 2

collapse :: Shape -> Tree -> Tree
collapse shape leaf@(Leaff ((xmin, ymin), (xmax, ymax)))
  | all (>= 0) values = Empty
  | otherwise = leaf
  where
      values = [shape (x, y) | x <- [xmin, xmax], y <- [ymin, ymax]]
collapse shape (Root a b c d) =
  collapse' $ map (collapse shape) [a, b, c, d]
    where
        сollapse' [Empty, Empty, Empty, Empty] = Empty
        collapse' [Full, Full, Full, Full] = Full
        collapse' [q, r, s, t] = Root q r s t
collapse _ t = t

lut :: [[(Side, Side)]]
lut = [[],                          -- 0000
       [(Upper,Right)],             -- 000d
       [(Left,Upper)],              -- 00c0
       [(Left,Right)],              -- 00cd
       [(Right,Lower)],             -- 0b00
       [(Upper,Lower)],             -- 0b0d
       [(Right,Lower),(Left,Upper)],-- 0bc0
       [(Left,Lower)],              -- 0bcd
       [(Lower,Left)],              -- a000
       [(Lower,Left),(Upper,Right)],-- a00d
       [(Lower,Upper)],             -- a0c0
       [(Lower,Right)],             -- a0cd
       [(Right,Left)],              -- ab00
       [(Upper,Left)],              -- ab0d
       [(Right,Upper)],             -- abc0
       []]                          -- abcd

index :: Shape -> Cell -> Int
index shape ((xmin, ymin), (xmax, ymax)) =
    sum [if shape pt < 0
         then 2^(3 - i)
         else 0
         | (pt, i) <- zip pts [0..]]
      where
        pts = [(x,y) | y <- [ymin, ymax], x <- [xmin, xmax]]

edges :: Shape -> Cell -> [(Side, Side)]
edges shape c = lut !! index shape c

pt :: Shape -> Cell -> Side -> Point
pt shape ((xmin, ymin), (xmax, ymax)) side =
    case side of
        Left  -> zero shape (xmin, ymin) (xmin, ymax)
        Right -> zero shape (xmax, ymin) (xmax, ymax)
        Lower -> zero shape (xmin, ymin) (xmax, ymin)
        Upper -> zero shape (xmin, ymax) (xmax, ymax)

zero :: Shape -> Point -> Point -> Point
zero s a@(ax, ay) b@(bx, by)
    | s a >= 0 = zero s b a
    | otherwise = zero' 0.5 0.25 10
    where
        pos f = (ax * (1-f) + bx * f, ay * (1-f) + by * f)
        zero' f step i
         | i == 0 = pos f
         | s (pos f) < 0 = zero' (f + step) (step / 2) (i - 1)
         | otherwise = zero' (f - step) (step / 2) (i - 1)

contours :: Shape -> Cell -> [Edge]
contours shape cell = [(pt' a, pt' b) |
                       (a, b) <- edges shape cell]
    where pt' = pt shape cell

----------------------------------------------------------------------------
listOfVectors :: [Particle] -> [Edge]
listOfVectors particles = foldMap (contours (shapes particles)) $ collapse (shapes particles) $ buildTree (-500,-500) (500, 500) 7

vectorsToPicture :: [Particle] -> [Picture]
vectorsToPicture particles = map (\e -> color blue (line (bimap realToFrac realToFrac (fst e) : [bimap realToFrac realToFrac (snd e)]))) (listOfVectors particles)
----------------------------------------------------------------------------

--listOfVectors :: [[Particle]] -> [Picture]
--listOfVectors particles = map (\p -> polygon (sortOn (distance (0,0)) (map position p))) particles

--vectorsToPicture :: [Particle] -> [Picture]
--vectorsToPicture particles = listOfVectors (dbscan (getParticleTree particles) 1 0 (map NotVisited particles))
--------------------------------------------------------------------------------