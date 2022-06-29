module TimeModule where

import Objects
import UsefulFunctions


applyVelocity :: Particle -> Float -> Particle
applyVelocity particle time =
  particle
    { position = (newX, newY)
    }
  where
    min_speed = minSpeed (config particle)
    (oldX, oldY) = position particle
    speed = vectorMagnitude (velocity particle)
    (speedX, speedY) = case speed >= min_speed of
      True -> velocity particle
      False -> (0, 0)

    newX = speedX * time + oldX
    newY = speedY * time + oldY

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
