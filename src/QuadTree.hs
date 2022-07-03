module QuadTree where

import Graphics.Gloss (Point)

data Circle = Circle Point Float deriving (Show, Eq)

-- | Check if point is inside circle specified by center and radius
inCircle :: Circle -> Point -> Bool
inCircle (Circle (centerX, centerY) radius) (x, y) =
  ((centerX - x) ^ 2 + (centerY - y) ^ 2) <= radius ^ 2

type Rectangle = (Point, Point)

-- | Check if point is inside rectangle specified by 2 points
inRectangle :: Rectangle -> Point -> Bool
inRectangle rectangle (x, y) = inX && inY
  where
    ((minX, minY), (maxX, maxY)) = properRectangle rectangle
    inX = minX <= x && x <= maxX
    inY = minY <= y && y <= maxY

-- | Construct rectangle such that:
-- * First point is lower left one
-- * Second point is upper right one
properRectangle :: Rectangle -> Rectangle
properRectangle ((x1, y1), (x2, y2)) =
  ((min x1 x2, min y1 y2), (max x1 x2, max y1 y2))

-- | Check if circle intersects rectangle
circleIntercectsRectangle :: Circle -> Rectangle -> Bool
circleIntercectsRectangle (Circle circleCenter radius) rectangle =
  or (inRectangles ++ inCircles)
  where
    ((minX, minY), (maxX, maxY)) = properRectangle rectangle
    horizontallyExtended = ((minX - radius, minY), (maxX + radius, maxY))
    verticallyExtended = ((minX, minY - radius), (maxX, maxY + radius))
    rectangularBoundaries = [horizontallyExtended, verticallyExtended]
    circularBoundaries = [(x, y) | x <- [minX, maxX], y <- [minY, maxY]]
    inRectangles = map (`inRectangle` circleCenter) rectangularBoundaries
    inCircles = map (\x -> inCircle (Circle x radius) circleCenter) circularBoundaries

-- | Each node stores 4 QuadTrees
-- | Each leaf contains at least 1 and no more than `capacity`
-- | elements in the list. Also each leaf has it's own boundaries
data QuadTree a
  = Node Rectangle [QuadTree a]
  | Leaf Int Rectangle [a]

instance Show a => Show (QuadTree a) where
  show (Leaf _ b l) = "{Leaf " ++ show b ++ " " ++ show l ++ "}\n"
  show (Node b l) = "{Node : " ++ mconcat (map show l) ++ "}\n"

-- | Returns children of given node of QuadTree
getChildren :: QuadTree a -> [QuadTree a]
getChildren Leaf {} = []
getChildren (Node _ children) = children

-- | Get list of all boundaries in the QuadTree given QuadTree
getBoundaries :: QuadTree a -> [Rectangle]
getBoundaries (Leaf _ boundary _) = [boundary]
getBoundaries (Node _ children) =
  foldr (\node boundaries -> boundaries ++ getBoundaries node) [] children

getBoundary :: QuadTree a -> Rectangle
getBoundary (Leaf _ boundary _) = boundary
getBoundary (Node boundary _) = boundary

-- | Construct QuadTree given capacity of leaves, initial boundaries,
-- | function to convert object to a point and list of objects
getTree :: Int -> Rectangle -> (a -> Point) -> [a] -> QuadTree a
getTree capacity boundaries = insertManyToQuadTree emptyTree
  where
    emptyTree = Leaf capacity boundaries []

-- | Insert new object to a QuadTree given existing QuadTree, function to
-- | convert object to a point and new object
insertToQuadTree :: QuadTree a -> (a -> Point) -> a -> QuadTree a
insertToQuadTree (Leaf capacity boundaries elements) getPosition element
  | length elements < capacity = Leaf capacity boundaries maybeInsertedElements
  | otherwise = insertToQuadTree subdivided getPosition element
  where
    subdivided = subdivideQuadTree getPosition (Leaf capacity boundaries elements)
    position = getPosition element
    maybeInsertedElements =
      if inRectangle boundaries position then element : elements else elements
insertToQuadTree (Node boundary trees) getPosition element =
  Node boundary newTrees
  where
    newTrees = mapFirst canInsert insert' trees
    canInsert t = inRectangle (getBoundary t) (getPosition element)
    insert' t = insertToQuadTree t getPosition element

-- | Applies function to the first element from the list that satisfies some property
mapFirst :: (a -> Bool) -> (a -> a) -> [a] -> [a]
mapFirst _ _ [] = []
mapFirst filt func (x : xs) =
  if filt x then func x : xs else x : mapFirst filt func xs

-- | Insert list of new objects to a QuadTree given existing QuadTree,
-- | function to convert object to a point and list of new objects
insertManyToQuadTree :: QuadTree a -> (a -> Point) -> [a] -> QuadTree a
insertManyToQuadTree tree getPosition = foldr insert' tree
  where
    insert' element tree = insertToQuadTree tree getPosition element

-- | Subdivide leaf into four leaves given function to convert object to a
-- | point and leaf of QuadTree
-- * It returns Node of QuadTree 4 leaves containing all the objects, that
--   were stored in given leaf
subdivideQuadTree :: (a -> Point) -> QuadTree a -> QuadTree a
subdivideQuadTree _ tree@(Node _ _) = tree
subdivideQuadTree getPosition leaf@(Leaf capacity boundaries elements) =
  insertManyToQuadTree (Node boundaries leaves) getPosition elements
  where
    leaves = map (\b -> Leaf capacity b []) (quarterBoundaries boundaries)

-- | Rectangle into 4 rectangles given boundaries of initial rectangle
quarterBoundaries :: Rectangle -> [Rectangle]
quarterBoundaries boundaries =
  [ (bottomLeft, middle),
    (topRight, middle),
    (bottomRight, middle),
    (topLeft, middle)
  ]
  where
    (bottomLeft, topRight) = properRectangle boundaries
    bottomRight = (fst topRight, snd bottomLeft)
    topLeft = (fst bottomLeft, fst topRight)
    boundaryList = [bottomLeft, topRight]
    midCoords f = sum (map f boundaryList) / 2
    middle = (midCoords fst, midCoords snd)

-- | Get list of objects that are located in specified radius given
-- | Quadtree, function to convert object to a point, point and radius
getObjectsInRadius :: QuadTree a -> (a -> Point) -> Point -> Float -> [a]
getObjectsInRadius (Leaf _ boundary objects) getPosition center radius =
  filter inCircleButNotCenter objects
    where
      inCircleButNotCenter object = inCircle (Circle center radius) pos && pos /= center
        where
          pos = getPosition object

getObjectsInRadius (Node boundary children) getPosition center radius =
  if circleIntercectsRectangle (Circle center radius) boundary
    then mconcat (map (\x -> getObjectsInRadius x getPosition center radius) children)
    else []

getElementsQuadTree :: QuadTree a -> [a]
getElementsQuadTree (Leaf _ _ objects) = objects
getElementsQuadTree (Node _ children) = concatMap getElementsQuadTree children
