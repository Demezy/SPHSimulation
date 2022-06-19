module RenderingOfUniverse where

import UsefulFunctions
import Graphics.Gloss
import Data.Tuple
import Objects

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
    iterations = 100
    renderCircle (radius, alpha) = color (newColor alpha) (thickCircle ((radius + 1) / 2) (radius + 1))
    circleTuple = zip listOfRadiuses listOfAlphas
    listOfRadiuses = [0 .. to]
    listOfAlphas = interpolationList (interpolation (\x -> x ^ 2 + 2 * x)) 0 1 iterations
    newColor a = makeColor r g b a
    rgba = rgbaOfColor oldColor
    r = firstOfTuple rgba
    g = secondOfTuple rgba
    b = thirdOfTuple rgba

-- | Render Particle.
renderParticle :: Particle -> Picture
renderParticle particle = pictures (render 0 radius oldColor)
  where
    oldColor = coloring (config particle)
    radius = 100

-- | Render Particle at given coordinates.
renderParticleAt :: Particle -> Picture
renderParticleAt particle = translate dx dy (renderParticle particle)
  where
    coordinate = position particle
    dx = fst coordinate
    dy = snd coordinate

-- | Render all Particles into Universe.
renderParticles :: [Particle] -> Picture
renderParticles particles = pictures (map renderParticleAt particles)

-- | Render Solid by itself.
renderSolid :: Solid -> Picture
renderSolid solid = rendering solid
  where
    rendering = renderFunction solid

-- | Render all Solids into Universe.
renderSolids :: [Solid] -> Picture
renderSolids solids = pictures (map renderSolid solids)

-- | Render whole Universe.
renderUniverse :: Universe -> Picture
renderUniverse universe = renderParticles particles <> renderSolids solids
  where
    particles = fluid universe
    solids = walls universe
