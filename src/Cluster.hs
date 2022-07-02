module Cluster where

import Graphics.Gloss
import QuadTree
import Objects
import Data.List
import Data.Bool
import SimulationModule


getParticleTree :: [Particle] -> QuadTree Particle
getParticleTree = getTree 4 ((-2000, -2000), (2000, 2000)) position

data Map = Visited Particle
         | NotVisited Particle
         | Noise      Particle
         deriving (Show)

instance Eq Map where
    p1 == p2 = position (particle p1) == position ( particle p2)

type Cluster = (QuadTree.Rectangle, [Particle])

particle :: Map -> Particle
particle (Visited p)    = p
particle (NotVisited p) = p
particle (Noise p)      = p

visited :: Map -> Bool
visited (NotVisited _) = False
visited (Noise _)      = False
visited _              = True

visit :: Map -> Map
visit = Visited . particle

noise :: Map -> Map
noise = Noise . particle

dbscan :: QuadTree Particle -> Float -> Int -> [Map] -> [[Particle]]
dbscan tree eps minPts points = map (fmap particle) cl
     where
         (cl, ns) = foldl constructClusers ([], []) points
         constructClusers (clusters, ns') c
             | or (fmap (c `elem`) clusters) = (clusters, ns')
             | otherwise  =
             case construction tree eps minPts points c of
               (Left n)           -> (clusters, n:ns')
               (Right newCluster) -> (clusters ++ [newCluster], ns')

construction :: QuadTree Particle -> Float -> Int -> [Map] -> Map -> Either Map [Map]
construction tree eps minPts points p =
     bool (Right (expansion tree [pVisited] [] eps minPts points)) (Left (noise pVisited)) (length neighbours < minPts)
     where
       pVisited  = visit p
       neighbours = findNeighbours tree (position (particle pVisited)) eps

expansion :: QuadTree Particle -> [Map] -> [Map] -> Float -> Int -> [Map] -> [Map]
expansion _ [] pss _ _ _ = pss
expansion tree (p:ps) pps eps minPts points =
     bool [] (expansion tree ps' pps' eps minPts points) (length neighbours >= minPts)
     where 
         neighbours = findNeighbours tree (position (particle p)) eps
         ps'        = ps `union` filter (`notElem` pps) (toMap points neighbours)
         pps'       = bool pps (p:pps) (p `notElem` pps)

toMap :: [Map] -> [Particle] -> [Map]
toMap m ps = filter (\p -> particle p `elem` ps) m

newtype MinPoint a = MinPoint Point

newtype MaxPoint a = MaxPoint Point

instance Semigroup (MinPoint a) where
  MinPoint (x1, y1) <> MinPoint (x2, y2) = MinPoint (min (x1 - 5) (x2 - 5), min (y1 - 5) (y2 - 5))

instance Semigroup (MaxPoint a) where
  MaxPoint (x1, y1) <> MaxPoint (x2, y2) = MaxPoint (max (x1 + 5) (x2 + 5), max (y1 + 5) (y2 + 5))

instance Monoid (MinPoint a) where
  mempty = MinPoint (1000000, 1000000)

instance Monoid (MaxPoint a) where
  mempty = MaxPoint (-1000000, -1000000)

findRectangle :: [QuadTree.Rectangle] -> QuadTree.Rectangle
findRectangle particles = (findMin particles, findMax particles)

findMin :: [QuadTree.Rectangle] -> Point
findMin []       = fromMinPoint mempty
findMin (x : xs) = fromMinPoint (MinPoint (fst x) <> MinPoint (findMin xs))

findMax :: [QuadTree.Rectangle] -> Point
findMax []       = fromMaxPoint mempty
findMax (x : xs) = fromMaxPoint (MaxPoint (snd x) <> MaxPoint (findMax xs))

fromMinPoint :: MinPoint a -> Point
fromMinPoint (MinPoint p) = p

fromMaxPoint :: MaxPoint a -> Point
fromMaxPoint (MaxPoint p) = p