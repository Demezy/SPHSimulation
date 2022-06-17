module ParticleModule where

import Graphics.Gloss
import Objects

renderParticle :: Particle -> Picture
renderParticle particle = coloredParticle
  where
    coloredParticle = color c (circle 20)
    c = coloring particle

renderParticleAt :: Particle -> Picture
renderParticleAt particle = translate dx dy (renderParticle particle)
  where
    point = position particle
    dx = fst point
    dy = snd point
