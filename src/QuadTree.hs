module QuadTree where

import Graphics.Gloss (Point)

sampleQuadTree = Leaf 4 (boundary 10) []

samplePoints :: [Point]
samplePoints =
  [ (-0, 1),
    (-1, 2),
    (-2, 3),
    (-3, 4),
    (-4, 5),
    (-5, 6),
    (-6, 7),
    (0, 0),
    (1, 0),
    (2, 0),
    (3, 0),
    (4, 0),
    (5, 0),
    (6, 0)
  ]

boundary t = ((-t, -t), (t, t))

-- | Each node stores 4 QuadTrees
-- | Each leaf contains at least 1 and no more than `capacity`
-- | elements in the list. Also each leaf has it's own boundaries
data QuadTree a
  = Node (Point, Point) [QuadTree a]
  | Leaf Int (Point, Point) [a]
  deriving (Show)

-- | Returns children of given node of QuadTree
getChildren :: QuadTree a -> [QuadTree a]
getChildren Leaf {} = []
getChildren (Node _ children) = children

-- | Construct QuadTree given capacity of leaves, initial boundaries,
-- | function to convert object to a point and list of objects
getTree :: Int -> (Point, Point) -> (a -> Point) -> [a] -> QuadTree a
getTree capacity boundaries = insertManyToQuadTree emptyTree
  where
    emptyTree = Leaf capacity boundaries []

-- | TODO fix inserting border points to multiple locations
-- | Insert new object to a QuadTree given existing QuadTree, function to
-- | convert object to a point and new object
insertToQuadTree :: QuadTree a -> (a -> Point) -> a -> QuadTree a
insertToQuadTree
  leaf@(Leaf capacity boundaries elements) getPosition
  element
    | length elements < capacity = Leaf capacity boundaries maybeInsertedElements
    | otherwise = insertToQuadTree (subdivideQuadTree getPosition leaf) getPosition element
    where
      position = getPosition element
      maybeInsertedElements =
        if inBoundaries boundaries position
          then element : elements
          else elements
insertToQuadTree (Node boundary trees) getPosition element =
  Node boundary (map (\t -> insertToQuadTree t getPosition element) trees)

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
quarterBoundaries :: (Point, Point) -> [(Point, Point)]
quarterBoundaries (a, b) =
  [ (bottomLeft, middle),
    (topRight, middle),
    (bottomRight, middle),
    (topLeft, middle)
  ]
  where
    bottomLeft = (min' fst a b, min' snd a b)
    topRight = (max' fst a b, max' snd a b)
    bottomRight = (max' fst a b, min' snd a b)
    topLeft = (min' fst a b, max' snd a b)
    middle = ((fst a + fst b) / 2, (snd a + snd b) / 2)
    min' f x y = min (f x) (f y)
    max' f x y = max (f x) (f y)

-- | Get list of all boundaries in the QuadTree given QuadTree
getBoundaries :: QuadTree a -> [(Point, Point)]
getBoundaries (Leaf _ boundary _) = [boundary]
getBoundaries (Node _ children) =
  foldr (\node boundaries -> boundaries ++ getBoundaries node) [] children

-- | Get list of objects that are located in specified radius given
-- | Quadtree, function to convert object to a point, point and radius
getObjectsInRadius :: QuadTree a -> (a -> Point) -> Point -> Float -> [a]
getObjectsInRadius (Leaf _ boundary objects) getPosition center radius =
  filter (inCircle center radius . getPosition) objects
getObjectsInRadius (Node boundary children) getPosition center radius =
  if circleIntercectsRectangle center radius boundary
    then mconcat (map (\x -> getObjectsInRadius x getPosition center radius) children)
    else []

circleIntercectsRectangle :: Point -> Float -> (Point, Point) -> Bool
circleIntercectsRectangle circleCenter radius rectangle = or (inRectangles ++ inCircles)
  where
    ((minX, minY), (maxX, maxY)) = properRectangle rectangle
    horizontallyExtended = ((minX - radius, minY), (maxX + radius, maxY))
    verticallyExtended = ((minX, minY - radius), (maxX, maxY + radius))
    rectangularBoundaries = [horizontallyExtended, verticallyExtended]
    circularBoundaries =
      [ (minX, minY),
        (maxX, minY),
        (minX, maxY),
        (maxX, maxY)
      ]
    inRectangles = map (`inBoundaries` circleCenter) rectangularBoundaries
    inCircles = map (\x -> inCircle x radius circleCenter) circularBoundaries

-- inRectangles = any (map (\x -> inBoundaries x circleCenter) [horizontallyExtended, verticallyExtended])

-- | Check if point is inside rectangle specified by 2 points
-- | given boundaries and point
inBoundaries :: (Point, Point) -> Point -> Bool
inBoundaries rectangle (x, y) = inX && inY
  where
    ((minX, minY), (maxX, maxY)) = properRectangle rectangle
    inX = minX <= x && x <= maxX
    inY = minY <= y && y <= maxY

inCircle :: Point -> Float -> Point -> Bool
inCircle (centerX, centerY) radius (x, y) = ((centerX - x) ^ 2 + (centerY - y) ^ 2) <= radius ^ 2

properRectangle :: (Point, Point) -> (Point, Point)
properRectangle ((x1, y1), (x2, y2)) = ((min x1 x2, min y1 y2), (max x1 x2, max y1 y2))

