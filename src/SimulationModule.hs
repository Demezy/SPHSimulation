module SimulationModule where

import Objects
import Graphics.Gloss

-- TODO Check value type
type Ai = (Particle -> Point -> Float)

-- TODO Implement or use existing func from any library
vectorDifference :: Point -> Point -> Vector
vectorDifference = undefined

-- TODO Implement
kernelFunction :: KernelFunc
kernelFunction = undefined

densityOfParticle :: Particle -> Float
densityOfParticle p = overallSum neighParticles sumElemI
    where
        pPos = position p
        pSmoothingLength = smoothingLength (config p)
        neighParticles = findNeighbors pPos pSmoothingLength

        rDiff :: Particle -> Vector
        rDiff pI = vectorDifference pPos (position pI)

        sumElemI :: Particle -> Float
        sumElemI pI = (mass (config pI)) * kernelFunction (rDiff pI) pSmoothingLength

        overallSum :: [Particle] -> (Particle -> Float) -> Float
        overallSum (p : ps) pFunc = (pFunc p) + (overallSum ps pFunc)

        -- appliedList = map sumElemI neighParticles
        -- overallSum = foldl (+) 0 appliedList


findNeighbors :: position -- ^ 
  -> smoothingLength -- ^ 
  -> [Particle]
findNeighbors = undefined

valueAtPoint :: position -> smoothingLength -> KernelFunc -> Ai -> value
valueAtPoint = undefined

pressureOfParticle :: Particle -> Environment -> Float
pressureOfParticle p env = pStiffness * (pDensity - envDensity)
    where
        envDensity = densityOfEnvironment env
        pDensity = densityOfParticle p
        pStiffness = stiffness (config p)



gravityForceOfParticle :: Particle -> Environment -> Force
gravityForceOfParticle particle env = (scalar * x, scalar * y)
 where
  g = gravityAcceleration env 
  massParticle = (mass . config) particle
  scalar = g * massParticle
  directionGravity = directionOfGravity env 
  x = fst directionGravity
  y = snd directionGravity

