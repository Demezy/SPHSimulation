module ParticleModule where

import Graphics.Gloss
import Objects

renderParticle :: Particle -> Picture
renderParticle _particle = circle 20

renderParticleAt :: Particle -> Picture
renderParticleAt particle = translate dx dy (renderParticle particle)
  where
    point = position particle
    dx = fst point
    dy = snd point
