module RenderingOfUniverse where

import Graphics.Gloss
import Objects
import Data.Tuple

interpolate :: Float -> Float -> Float -> Float
interpolate from to phase = ((to - from) * phase + from) / 100

render :: Float -> Float -> Color -> [Picture]
render from to oldColor = map (color newColor . renderParticle) listOfRadiuses
  where
   -- phases = reverse (take ((floor to) * 20) (iteration from to to))
   -- iteration from to phase = (interpolate from to phase) : iteration from to (phase - (to / 10))
    renderParticle radius = thickCircle ((radius + 1) / 2) (radius + 1)
    listOfRadiuses = [0..to]
    rgba     = rgbaOfColor oldColor
    newColor = makeColor r g b a
    r = f rgba
    g = s rgba
    b = t rgba
    a = 1 / to

-- | First element in four elements tuple. 
f :: (a,b,c,d) -> a
f (x, _, _, _) = x

-- | Second element in four elements tuple. 
s :: (a,b,c,d) -> b
s (_, x, _, _) = x

-- | Third element in four elements tuple. 
t :: (a,b,c,d) -> c
t (_, _, x, _) = x

-- | Render Particle.
renderParticle :: Particle -> Picture
renderParticle particle = pictures (render 0 radius colorr)
  where
    colorr = coloring (config particle)
    radius = 100

-- | Render Particle at given coordinates.
renderParticleAt :: Particle -> Picture
renderParticleAt particle = translate dx dy(renderParticle particle)
  where
    coordinate = position particle
    dx         = fst coordinate
    dy         = snd coordinate

-- | Render fluid from all Universe.
renderParticles :: Universe -> Picture
renderParticles universe = pictures(map renderParticleAt particles)
  where
    particles = fluid universe

