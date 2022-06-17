module ParticleModule where

import Graphics.Gloss
import Objects

renderParticleAt :: Particle -> Picture
renderParticleAt particle = translate dx dy (rendering particle)
  where
    rendering = renderParticle particle
    point = position particle
    dx = fst point
    dy = snd point
