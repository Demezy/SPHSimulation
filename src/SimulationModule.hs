module SimulationModule where

import Objects
import Graphics.Gloss

type Ai = (Particle -> Point -> Float)

vectorDifference :: Point -> Point -> Vector
vectorDifference (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

vectorMagnitude :: Vector -> Float
vectorMagnitude (x, y) = sqrt (x**2 + y**2)

distance :: Point -> Point -> Float
distance points = vectorMagnitude.vectorDifference points

kernelFunction0 :: KernelFunc
kernelFunction0 r h
  | 0 <= r && r <= h = (315 / (64 * pi * h**9)) * (h**2 - r**2)**3
  | otherwise = 0

kernelFunction1 :: KernelFunc
kernelFunction1 r h
  | 0 <= r && r <= h = (-(315 / (64 * pi * h**9))) * (6 * r) * (h**2 - r**2)**2
  | otherwise = 0

kernelFunction2 :: KernelFunc
kernelFunction2 r h
  | 0 <= r && r <= h = (315 / (64 * pi * h**9)) * 6 * (h**2 - r**2) * (4 * r**2 - (h**2 - r**2))
  | otherwise = 0

kernelFunctionIncompressible  :: KernelFunc
kernelFunctionIncompressible  r h
  | 0 <= r && r <= h = (1 - r/h)**2
  | otherwise = 0


findNeighbours :: [Particle] -> Point -> Float -> [Particle]
findNeighbours pList point h = filter filterFunc pList
  where
    filterFunc particle = (position particle /= point) && (distance point (position particle) <= h)

valueAtPoint :: position -> smoothingLength -> KernelFunc -> Ai -> value
valueAtPoint = undefined

pressureOfParticle :: [Particle] -> Particle -> Environment -> Float
pressureOfParticle pList p env = pStiffness * (pDensity - envDensity)
    where
        envDensity = densityOfEnvironment env
        pDensity = densityOfParticle pList p
        pStiffness = stiffness (config p)


-- Base function to calculate formula SumI(W(...)*m*(...))
baseSumFormula :: [Particle] -- pList
               -> Particle -- particle
               -> ([Particle] -> Particle -> Float) -- pass densityOfParticle by default
               -> KernelFunc
               -> (Particle -> Float) -- Ai
               -> Float
baseSumFormula pList p densityF kernelFunc ai = result neighParticles resI
    where
        pPos = position p
        pSmoothingLength = smoothingLength (config p)
        neighParticles = findNeighbours pList pPos pSmoothingLength

        rDiff :: Particle -> Vector
        rDiff pI = vectorDifference pPos (position pI)

        resI :: Particle -> Float
        resI pI = (mass (config pI) * (ai pI) / (densityF pList pI))
                        * kernelFunc (vectorMagnitude (rDiff pI)) pSmoothingLength

        result :: [Particle] -> (Particle -> Float) -> Float
        result (p : ps) pFunc = pFunc p + result ps pFunc

densityOfParticle :: [Particle] -> Particle -> Float
densityOfParticle pList p = baseSumFormula pList p densityF kernelFunc ai
    where
        kernelFunc = densityKernel (config p)
        densityF = (\_ _ -> 1.0)
        ai = (\_ -> 1.0)

pressureAtPoint :: [Particle] -> Particle -> Environment -> Float
pressureAtPoint pList p env = baseSumFormula pList p densityOfParticle kernelFunc ai
    where
        kernelFunc = pressureKernel (config p)
        ai pI = pressureOfParticle pList pI env


gravityForceOfParticle :: Particle -> Environment -> Force
gravityForceOfParticle particle env = (scalar * x, scalar * y)
 where
  g = gravityAcceleration env 
  massParticle = (mass . config) particle
  scalar = g * massParticle
  directionGravity = directionOfGravity env 
  x = fst directionGravity
  y = snd directionGravity

