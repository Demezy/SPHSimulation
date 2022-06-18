{-# OPTIONS_GHC -fdefer-typed-holes -fshow-hole-constraints -funclutter-valid-hole-fits #-}

module SimulationModule where

import Objects
import Graphics.Gloss

-- TODO Check value type
type Ai = (Particle -> Point -> Float)

densityOfParticle :: Particle -> value
densityOfParticle = _

findNeighbours :: position -> smoothingLength -> [Particle]
findNeighbours = _

valueAtPoint :: position -> smoothingLength -> KernelFunc -> Ai -> value
valueAtPoint = _

pressureOfParticle :: Particle -> Environment -> value
pressureOfParticle = _

gravityForceOfParticle :: Particle -> Environment -> Force
gravityForceOfParticle = _