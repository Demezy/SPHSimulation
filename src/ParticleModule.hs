module ParticleModule where

import Graphics.Gloss
import Objects

renderParticle :: Particle -> Picture
renderParticle particle = translate dx dy (circle 80)
  where
    point = position particle
    dx = fst point
    dy = snd point
