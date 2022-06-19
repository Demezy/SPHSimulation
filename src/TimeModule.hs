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

applyForce :: Particle -> Force -> Float -> Particle
applyForce particle force time =
  particle
    { velocity = (newVX, newVY)
    }
  where
    newVX = deltaVX + oldVX
    newVY = deltaVY + oldVY
    deltaVX = time * (forceX / massParticle)
    deltaVY = time * (forceY / massParticle)
    (forceX, forceY) = force
    (oldVX, oldVY) = velocity particle
    massParticle = (mass . config) particle
