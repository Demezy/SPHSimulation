{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Test where

import Data.Monoid
import Data.Foldable hiding (toList)
import Data.Maybe

import Prelude hiding (Left, Right)
import Numeric.LinearAlgebra.HMatrix hiding (inv)
import Graphics.Gloss
import Data.Bifunctor

import Objects


type P = (Double, Double)
type Shape = P -> Double

circlee :: P -> Double -> Shape
circlee (x0, y0) r (x, y) = sqrt ((x0 - x)^2 + (y0 - y)^2) - r

left :: Double -> Shape
left x0 (x, _) = x - x0

right :: Double -> Shape
right x0 (x, _) = x0 - x

lower :: Double -> Shape
lower y0 (_, y) = y - y0

upper :: Double -> Shape
upper y0 (_, y) = y0 - y

(∪) :: Shape -> Shape -> Shape
a ∪ b = \p -> min (a p) (b p)

(∩) :: Shape -> Shape -> Shape
a ∩ b  = \p -> max (a p) (b p)

type Min = P
type Max = P

hi :: [Particle] -> Shape
hi = r 

r [x]      = circlee (realToFrac dx, realToFrac dy) 5
    where
        coordinate = position x
        dx = fst coordinate
        dy = snd coordinate

r (x : xs) = circlee (realToFrac dx, realToFrac dy) 5 ∪ r xs
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
    where collapse' [Empty, Empty, Empty, Empty] = Empty
          collapse' [Full, Full, Full, Full] = Full
          collapse' [q, r, s, t] = Root q r s t
collapse _ t = t

--------------------------------------------------------------------------------

instance Foldable Tree_ where
    foldMap f (Leaf a) = f a
    foldMap f (Root a b c d) = mconcat $ map (foldMap f) [a, b, c, d]
    foldMap _ _ = mempty

--------------------------------------------------------------------------------

-- foldTree tree quad root empty full leaf
foldTree :: Monoid m => Tree -> Cell -> (Cell -> m) -> (Cell -> m)
                                     -> (Cell -> m) -> (Cell -> m) -> m
foldTree (Root a b c d) q@((xmin, ymin), (xmax, ymax)) root empty full leaf =
    mconcat [root q, foldTree a ((xmin, ymin), (xmid, ymid)) root empty full leaf,
                     foldTree b ((xmid, ymin), (xmax, ymid)) root empty full leaf,
                     foldTree c ((xmin, ymid), (xmid, ymax)) root empty full leaf,
                     foldTree d ((xmid, ymid), (xmax, ymax)) root empty full leaf]
    where xmid = (xmin + xmax) / 2
          ymid = (ymin + ymax) / 2

foldTree Empty q _ f _ _ = f q
foldTree Full  q _ _ f _ = f q
foldTree (Leaf _) q _ _ _ f = f q

--------------------------------------------------------------------------------

data Side = Upperr | Lowerr | Left | Right deriving Show

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
    sum [if shape pt < 0 then 2^(3 - i) else 0 |
         (pt, i) <- zip pts [0..]]
    where pts = [(x,y) | y <- [ymin, ymax], x <- [xmin, xmax]]

edges :: Shape -> Cell -> [(Side, Side)]
edges shape c = lut !! index shape c

pt :: Shape -> Cell -> Side -> P
pt shape ((xmin, ymin), (xmax, ymax)) side =
    case side of Left  -> zero shape (xmin, ymin) (xmin, ymax)
                 Right -> zero shape (xmax, ymin) (xmax, ymax)
                 Lowerr -> zero shape (xmin, ymin) (xmax, ymin)
                 Upperr -> zero shape (xmin, ymax) (xmax, ymax)

zero :: Shape -> P -> P -> P
zero s a@(ax, ay) b@(bx, by)
    | s a >= 0 = zero s b a
    | otherwise = zero' 1 1 10
    where pos f = (ax * (1-f) + bx * f, ay * (1-f) + by * f)
          zero' f step i
           | i == 0 = pos f
           | s (pos f) < 0 = zero' (f + step) (step / 2) (i - 1)
           | otherwise = zero' (f - step) (step / 2) (i - 1)

type Edge = (P, P)

contours :: Shape -> Cell -> [Edge]
contours shape cell = [(pt' a, pt' b) |
                       (a, b) <- edges shape cell]
    where pt' = pt shape cell

--------------------------------------------------------------------------------
merge :: Shape -> Tree -> Tree
merge shape (Root a b c d) =
    merge' a' b' c' d'
    where [a', b', c', d'] = map (merge shape) [a, b, c, d]
          merge' (Leaf (min, i)) (Leaf (q, r))
                 (Leaf (s, t)) (Leaf (_, max)) =
            let scores = map (score shape (min, max))
                         [i, q, r, s, t]
            in if all (< 0.001) scores
               then Leaf (min, max)
               else Root a' b' c' d'
          merge' _ _ _ _ = Root a' b' c' d'
merge _ t = t

interpolate :: Shape -> Cell -> P -> Double
interpolate shape ((xmin, ymin), (xmax, ymax)) (x,y) =
    let dx = (x - xmin) / (xmax - xmin)
        dy = (y - ymin) / (ymax - ymin)
        ab = shape (xmin, ymin) * (1 - dx) +
             shape (xmax, ymin) * dx
        cd = shape (xmin, ymax) * (1 - dx) +
             shape (xmax, ymax) * dx
    in ab * (1 - dy) + cd * dy

score :: Shape -> Cell -> P -> Double
score shape cell pt = abs $ interpolate shape cell pt - shape pt

--------------------------------------------------------------------------------

deriv :: Shape -> P -> P
deriv shape (x,y) =
    let epsilon = 0.001
        dx = shape (x + epsilon, y) - shape (x - epsilon, y)
        dy = shape (x, y + epsilon) - shape (x, y - epsilon)
        len = sqrt $ dx^2 + dy^2
    in (dx / len, dy / len)

feature :: Shape -> Cell -> Maybe P
feature shape cell =
    if length pts_ >= 2 then
        let pts = map fromTuple pts_
            nms = map (fromTuple . deriv shape) pts_
            center = sum pts / fromIntegral (length pts)

            a = fromRows nms
            b = col $ zipWith (\pt nm -> (pt - center) <·> nm) pts nms

            p = center + head (toColumns $ linearSolveSVD a b)
        in Just $ (\ [x, y] -> (x, y)) $ toList p
    else Nothing
    where pts_ = concatMap (\(a,b) -> [a,b]) $ contours shape cell
          fromTuple = \(x,y) -> fromList [x,y]

--------------------------------------------------------------------------------

dc :: Shape -> Tree -> [Edge]
dc = faceProc

faceProc :: Shape -> Tree -> [Edge]
faceProc shape (Root a b c d) =
    concatMap (faceProc shape) [a,b,c,d] ++
    edgeProcH shape a b ++ edgeProcH shape c d ++
    edgeProcV shape a c ++ edgeProcV shape b d
faceProc _ _ = []

edgeProcH :: Shape -> Tree -> Tree -> [Edge]
edgeProcH shape (Leaf a) (Leaf b) = [(fromJust $ feature shape a,
                                      fromJust $ feature shape b)]
edgeProcH shape leaf@(Leaf _) (Root a _ c _) =
    edgeProcH shape leaf a ++ edgeProcH shape leaf c
edgeProcH shape (Root _ b _ d) leaf@(Leaf _) =
    edgeProcH shape b leaf ++ edgeProcH shape d leaf
edgeProcH shape (Root _ b _ d) (Root a _ c _) =
    edgeProcH shape b a ++ edgeProcH shape d c
edgeProcH _ _ _ = []

edgeProcV :: Shape -> Tree -> Tree -> [Edge]
edgeProcV shape (Leaf a) (Leaf b) = [(fromJust $ feature shape a,
                                      fromJust $ feature shape b)]
edgeProcV shape (Root _ _ c d) leaf@(Leaf _) =
    edgeProcV shape c leaf ++ edgeProcV shape d leaf
edgeProcV shape leaf@(Leaf _) (Root a b _ _) =
    edgeProcV shape leaf a ++ edgeProcV shape leaf b
edgeProcV shape (Root _ _ c d) (Root a b _ _) =
    edgeProcV shape c a ++ edgeProcV shape d b
edgeProcV _ _ _ = []

f :: [Particle] -> [Edge]
f particles = dc (hi particles) $ collapse (hi particles) $ merge (hi particles) $ buildTree (-500,-500) (500, 500) 11

ff :: [Particle] -> [Picture]
ff particles = map (\e -> line (bimap realToFrac realToFrac (fst e) : [bimap realToFrac realToFrac (snd e)])) (f particles)
