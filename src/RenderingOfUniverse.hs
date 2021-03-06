module RenderingOfUniverse where

import Data.Tuple
import Graphics.Gloss
import Objects
import UsefulFunctions
import Metaballs
import QuadTree
import Debug.Trace
import TotalConfig
import Graphics.Gloss.Interface.Pure.Game (Picture(Polygon))

-- | This is standart interpolation functino that returns
--   value between 2 given ones
-- | Arguments:
-- * Function that joins values from and to
-- * From - starting value
-- * To - ending value
-- * Phase determines position between from and to (if phase not in [0,1] it
--   is bounded to the nearest value in [0,1])
interpolation :: (Float -> Float) -> Float -> Float -> Float -> Float
interpolation func from to phase
  | phase < 0 = from
  | phase > 1 = to
  | otherwise = func ((to - from) * phase + from)

-- | This function produces list of values between 2 ones
-- | Arguments:
-- * Interpolation function
-- * From - starting value
-- * To - ending value
-- * Iterations - number of elements evenly spaced between from and to
interpolationList :: (Float -> Float -> Float -> Float) -> Float -> Float -> Int -> [Float]
interpolationList intFunction from to iterations = reverse (constuctList [])
  where
    constuctList l
      | null l = constuctList [intFunction from to (step 1)]
      | length l < iterations = constuctList (intFunction from to (step (length l)) - sum l : l)
      | otherwise = l
    step n = fromIntegral n / fromIntegral iterations

-- | Create a picture by using interpolation function.
render :: Float -> Float -> Color -> [Picture]
render from to oldColor = map renderCircle circleTuple
  where
    iterations = 10
    renderCircle (radius, alpha) = color (newColor alpha) (thickCircle ((radius + 1) / 2) (radius + 1))
    circleTuple = zip listOfRadiuses listOfAlphas
    listOfRadiuses = [10 .. 11]
    listOfAlphas = interpolationList (interpolation (\x -> sqrt(cos x) * cos(300 * x))) 0 1 iterations
    newColor a = makeColor r g b a
    rgba = rgbaOfColor oldColor
    r = firstOfTuple rgba
    g = secondOfTuple rgba
    b = thirdOfTuple rgba


renderParticleSmoothingCircle :: Particle -> Picture
renderParticleSmoothingCircle particle
  | displaySmoothingLength ourProgramConfig = smoothingCircle
  | otherwise = blank
  where
    ss = smoothingLength (config particle)
    smoothingCircle = color red (circle ss)

renderForceOfParticle :: Particle -> Picture
renderForceOfParticle particle 
  | showForces ourProgramConfig = pictures [pv, vv, tv, fv, gv] 
  | otherwise = blank
  where
    pv = color red (linePic (pf particle))
    vv = color blue (linePic (vf particle))
    tv = color yellow (linePic (tf particle))
    fv = color green (linePic (ff particle))
    gv = color magenta (linePic (gf particle))
    lineVec f = vectorMul f (forcesScalarVal ourProgramConfig)
    linePic f = drawLine vec
      where
        vec = lineVec f

arrowHead :: Picture
arrowHead = scale k k square 
  where 
    k = 5
    square' = [
      ( 1, -1),
      ( 1,  1),
      (-1,  1),
      (-1, -1)
      ]
    square = Polygon square'

drawLine :: Point -> Picture 
drawLine position = line [(0, 0), position]

renderVelocityOfParticle :: Particle -> Picture
renderVelocityOfParticle particle
 | displayVelocityVector ourProgramConfig =
    color violet (drawLine (x, y) <> translate x y arrowHead)
 | otherwise = blank
    where
      (x, y) = vectorMul (velocity particle) (velocityScalarVal  ourProgramConfig) 
      -- (x, y) = velocity particle

renderParticleItself :: Particle -- ^ 
  -> Picture
renderParticleItself particle = pictures (render 0 r oldColor)
  where
    oldColor = coloring (config particle)
    r = radius particle
renderParticle particle
  = renderForceOfParticle particle 
  <> renderVelocityOfParticle particle
  <> renderParticleSmoothingCircle particle
  <> renderParticleItself particle

-- | Render Particle at given coordinates.
renderParticleAt :: Particle -> Picture
renderParticleAt particle = translate dx dy (renderParticle particle)
  where
    coordinate = position particle
    dx = fst coordinate
    dy = snd coordinate

renderParticles :: [Particle] -> Picture
renderParticles ps
  = renderParticlesAsCircles ps
  <> renderParticlesAsMesh ps

renderParticlesAsCircles :: [Particle] -> Picture
renderParticlesAsCircles ps 
  |circleRenderMode ourProgramConfig =  pictures (map renderParticleAt ps)
  | otherwise = blank

renderParticlesAsMesh :: [Particle] -> Picture  
renderParticlesAsMesh ps 
  | meshRenderMode ourProgramConfig = pictures( vectorsToPicture  ps)
  | otherwise = blank

-- | Render Solid by itself.
renderWall :: Wall -> Picture
renderWall wall = rendering wall
  where
    rendering = renderFunc wall

-- | Render all Solids into Universe.
renderWalls :: [Wall] -> Picture
renderWalls solids = pictures (map renderWall solids)

renderUniverseInfo :: Universe -> Picture
renderUniverseInfo universe 
  | displayUniverseInfo ourProgramConfig = moveToTopLeft (renderValueList (zip (map show [1..]) (map show (fluid universe))))
  | otherwise = blank
  where
    margin = 20
    scale = 0.1
    renderValue (name, value) = Color white (Scale scale scale (Text (name ++ ": " ++ show value)))
    moveToTopLeft = translate (-700) 400
    moveDown = translate 0 (-margin)
    renderValueList = foldr (\value rendered -> moveDown rendered <> renderValue value) blank

debugTree :: QuadTree Particle -> Picture
debugTree tree 
  | displayQuadTree ourProgramConfig = pictures (map renderBoundaries boundaries)
  | otherwise = blank
  where
    boundaries = map properRectangle (getBoundaries tree)
    renderBoundaries (p1, p2) = color red (line [p1, (fst p2, snd p1), p2, (fst p1, snd p2)])

-- | Render whole Universe.
renderUniverse :: Universe -> Picture
renderUniverse universe
  = renderParticles particles
  <> renderWalls solids
  <> debugTree (fluidAsTree universe)
  <> renderUniverseInfo universe
  where
    particles = fluid universe
    solids = walls universe
