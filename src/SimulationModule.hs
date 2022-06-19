{-# OPTIONS_GHC -fdefer-typed-holes -fshow-hole-constraints -funclutter-valid-hole-fits #-}

module SimulationModule where

import Objects
import Graphics.Gloss

-- TODO Check value type
type Ai = (Particle -> Point -> Float)

densityOfParticle :: Particle -> value
densityOfParticle = undefined 

findNeighbours :: position -> smoothingLength -> [Particle]
findNeighbours = undefined

valueAtPoint :: position -> smoothingLength -> KernelFunc -> Ai -> value
valueAtPoint = undefined

pressureOfParticle :: Particle -> Environment -> value
pressureOfParticle = undefined

gravityForceOfParticle :: Particle -> Environment -> Force
gravityForceOfParticle = undefined
