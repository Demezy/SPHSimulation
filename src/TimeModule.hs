module TimeModule where

import Graphics.Gloss
import Objects

applyVelocity :: Particle -> Float -> Particle
applyVelocity particle time =
  particle
    { position = (newX, newY)
    }
  where
    newX = speedX * time + oldX
    newY = speedY * time + oldY
    (speedX, speedY) = velocity particle
    (oldX, oldY) = position particle
