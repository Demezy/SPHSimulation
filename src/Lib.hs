module Lib
    ( glossExample
    ) where

import Graphics.Gloss
import ParticleModule
import Objects

window :: Display
window = InWindow "Nice Window" (1700, 1900) (10, 10)

background :: Color
background = white

drawing :: Picture
drawing = circle 80

sampleParticle :: Particle
sampleParticle = Particle {
    position= (1, 1)
  , velocity = (1, 1)
  , config = undefined
  , renderParticle = rendering 
  }

sampleParticle2 :: Particle
sampleParticle2 = Particle {
    position= (90, 0)
  , velocity = (1, 1)
  , config = undefined
  , renderParticle = rendering 
  }

rendering :: Particle -> Picture
rendering particle = coloredParticle
  where
    coloredParticle = color black (thickCircle 20 90)



glossExample :: IO ()
glossExample = display window background ((renderParticleAt sampleParticle) <> (renderParticleAt sampleParticle2))
-- glossExample = display window background drawing
