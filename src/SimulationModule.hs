module SimulationModule where

import Objects

densityOfParticle :: Particle -> Value
densityOfParticle = _

findNeighbours :: Position -> smoothingLength -> [Particle]
findNeighbours = _

valueAtPoint :: Position -> smothering Length -> Kernel function -> Ai (see below) -> value
valueAtPoint = _

pressureOfParticle :: Particle -> Environment -> value
pressureOfParticle = _

Ai :: (Particle -> Position -> value)
Ai = _

gravityForceOfParticle :: Particle -> Environment -> Force
gravityForceOfParticle = _