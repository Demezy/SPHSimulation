module Lib
    ( glossExample
    ) where

import Graphics.Gloss
import ParticleModule
import Objects

window :: Display
window = InWindow "Nice Window" (200, 200) (10, 10)

background :: Color
background = white

drawing :: Picture
drawing = circle 80

sampleParticle :: Particle
sampleParticle = Particle {
    position= (1, 1)
  , velocity = (1, 1)
  , mass = 42 
  , config = undefined
  , coloring = black
  }

sampleParticle2 :: Particle
sampleParticle2 = Particle {
    position= (5, 5)
  , velocity = (1, 1)
  , mass = 42 
  , config = undefined
  , coloring = yellow
  }


glossExample :: IO ()
glossExample = display window background ((renderParticleAt sampleParticle) <> (renderParticleAt sampleParticle2))
-- glossExample = display window background drawing
