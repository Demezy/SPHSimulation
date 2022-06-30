module Test where

import Data.Monoid
import Data.Foldable
    ( Foldable(length, foldMap, sum), all, concatMap )
import Data.Maybe

import Prelude hiding (Left, Right)
import Numeric.LinearAlgebra.HMatrix hiding (inv)
import Graphics.Gloss
import Data.Bifunctor

import Objects
import Graphics.Gloss.Geometry.Line


type Shape = Point -> Float

circlee :: Point -> Float -> Shape
circlee (x0, y0) r (x, y) = sqrt ((x0 - x)^2 + (y0 - y)^2) - r

(∪) :: Shape -> Shape -> Shape
a ∪ b = \p -> min (a p) (b p)

type Min = Point
type Max = Point

hi :: [Particle] -> Shape
hi = r 

r [x]      = circlee (dx, dy) 2
    where
        coordinate = position x
        dx = fst coordinate
        dy = snd coordinate
r (x : xs) = circlee (dx, dy) 2 ∪ r xs
    where
        coordinate = position x
        dx = fst coordinate
        dy = snd coordinate

--------------------------------------------------------------------------------

-- General rule of quadtree ordering:
--   2-------3
--   | 2 | 3 |
--   ---------
--   | 0 | 1 |
--   0-------1


data Tree_ a = Root (Tree_ a) (Tree_ a) (Tree_ a) (Tree_ a) |
               Empty | Full | Leaf a
     deriving (Show, Eq)

type Cell = (Min, Max)
type Tree = Tree_ Cell

buildTree :: Min -> Max -> Int -> Tree
buildTree min max 0 = Leaf (min, max)
buildTree (xmin, ymin) (xmax, ymax) i =
    Root (buildTree (xmin, ymin) (xmid, ymid) (i - 1))
         (buildTree (xmid, ymin) (xmax, ymid) (i - 1))
         (buildTree (xmin, ymid) (xmid, ymax) (i - 1))
         (buildTree (xmid, ymid) (xmax, ymax) (i - 1))
    where xmid = (xmin + xmax) / 2
          ymid = (ymin + ymax) / 2

collapse :: Shape -> Tree -> Tree
collapse shape leaf@(Leaf ((xmin, ymin), (xmax, ymax)))
  | all (< 0) values = Full
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

--------------------------------------------------------------------------------

instance Foldable Tree_ where
    foldMap f (Leaf a) = f a
    foldMap f (Root a b c d) = mconcat $ map (foldMap f) [a, b, c, d]
    foldMap _ _ = mempty
    
--------------------------------------------------------------------------------

data Side = Upperr | Lowerr | Left | Right 
  deriving Show

-- This lookup table takes a bitmask abcd and
-- returns a list of edges between which we
-- should draw contours (to outline the shape)
lut :: [[(Side, Side)]]
lut = [[],                          -- 0000
       [(Upperr,Right)],             -- 000d
       [(Left,Upperr)],              -- 00c0
       [(Left,Right)],              -- 00cd
       [(Right,Lowerr)],             -- 0b00
       [(Upperr,Lowerr)],             -- 0b0d
       [(Right,Lowerr),(Left,Upperr)],-- 0bc0
       [(Left,Lowerr)],              -- 0bcd
       [(Lowerr,Left)],              -- a000
       [(Lowerr,Left),(Upperr,Right)],-- a00d
       [(Lowerr,Upperr)],             -- a0c0
       [(Lowerr,Right)],             -- a0cd
       [(Right,Left)],              -- ab00
       [(Upperr,Left)],              -- ab0d
       [(Right,Upperr)],             -- abc0
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
        Lowerr -> zero shape (xmin, ymin) (xmax, ymin)
        Upperr -> zero shape (xmin, ymax) (xmax, ymax)

zero :: Shape -> Point -> Point -> Point
zero s a@(ax, ay) b@(bx, by)
    | s a >= 0 = zero s b a
    | otherwise = zero' 0.5 0.25 10
    where pos f = (ax * (1-f) + bx * f, ay * (1-f) + by * f)
          zero' f step i
           | i == 0 = pos f
           | s (pos f) < 0 = zero' (f + step) (step / 2) (i - 1)
           | otherwise = zero' (f - step) (step / 2) (i - 1)

type Edge = (Point, Point)

contours :: Shape -> Cell -> [Edge]
contours shape cell = [(pt' a, pt' b) |
                       (a, b) <- edges shape cell]
    where pt' = pt shape cell

----------------------------------------------------------------------------

listOfVectors particles =  (foldMap (contours (hi particles)) $ collapse (hi particles)$ buildTree (-500,-500) (500, 500) 9 )

vectorsToPicture :: [Particle] -> [Picture]
vectorsToPicture particles = map (\e -> line (bimap realToFrac realToFrac (fst e) : [bimap realToFrac realToFrac (snd e)])) (listOfVectors particles)
