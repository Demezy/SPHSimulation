module SimulationModule where

import Objects

Ai :: (Particle -> Position -> value)
Ai = _

densityOfParticle :: Particle -> Value
densityOfParticle = _

findNeighbours :: Position -> smoothingLength -> [Particle]
findNeighbours = _

valueAtPoint :: Position -> smothering Length -> Kernel function -> Ai (see below) -> value
valueAtPoint = _

pressureOfParticle :: Particle -> Environment -> value
pressureOfParticle = _

gravityForceOfParticle :: Particle -> Environment -> Force
gravityForceOfParticle = _