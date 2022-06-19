module RenderingOfUniverse where

import Graphics.Gloss
import Objects
import Data.Tuple
import UsefulFunctions

interpolate :: Float -> Float -> Float -> Float
interpolate from to phase = ((to - from) * phase + from) / 100

render :: Float -> Float -> Color -> [Picture]
render from to oldColor = map (color newColor . renderParticle) listOfRadiuses
  where
   -- phases = reverse (take ((floor to) * 20) (iteration from to to))
   -- iteration from to phase = (interpolate from to phase) : iteration from to (phase - (to / 10))
    renderParticle radius = thickCircle ((radius + 1) / 2) (radius + 1)
    listOfRadiuses        = [0..to]
    newColor = makeColor r g b a
    rgba     = rgbaOfColor oldColor
    r        = firstOfTuple rgba
    g        = secondOfTuple rgba
    b        = thirdOfTuple rgba
    a        = 1 / to

-- | Render Particle.
renderParticle :: Particle -> Picture
renderParticle particle = pictures (render 0 radius oldColor)
  where
    oldColor = coloring (config particle)
    radius   = 100

-- | Render Particle at given coordinates.
renderParticleAt :: Particle -> Picture
renderParticleAt particle = translate dx dy(renderParticle particle)
  where
    coordinate = position particle
    dx         = fst coordinate
    dy         = snd coordinate

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
renderUniverse universe = renderParticles particles
  where
    particles = fluid universe
    solids    = walls universe
