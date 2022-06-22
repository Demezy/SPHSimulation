module SimulationModule where

import Graphics.Gloss
import Objects
import UsefulFunctions

type Ai = (Particle -> Particle -> Vector)

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

particleValue :: [Particle]  -- Fluid
              -> Particle    -- Main particle
              -> KernelFunc  -- kernel function
              -> Ai          -- function to calculate force between 2 particles
              -> Vector      -- resulting force vector
particleValue particles particleI kernelFunc func = vectorSum (map itemResult neighbours)
  where
    h = smoothingLength (config particleI)
    neighbours = findNeighbours particles (position particleI) h

    itemResult particleJ = vectorMul itemForce (mass (config particleJ) * kernelFunc r h)
      where
        r = distance (position particleI) (position particleJ)
        itemForce = func particleI particleJ

particleDensity :: [Particle]  -- Fluid
                -> Particle    -- Main particle
                -> Float       -- resulting density
particleDensity particles particleI = fst (particleValue particles particleI kernelFunc (\_ _ -> (1, 0)))
  where
    kernelFunc = densityKernel (config particleI)

particlePressure :: Particle    -- Main particle
                 -> Float       -- environment density
                 -> Float       -- density at main particle
                 -> Float       -- resulting pressure
particlePressure particleI envDensity densityI = k * (densityI - envDensity)
  where
    k = stiffness (config particleI)

pressureForceFunc :: [Particle] -> Float -> Ai
pressureForceFunc particles envDensity particleI particleJ = forceVector
  where
    densityI = particleDensity particles particleI
    densityJ = particleDensity particles particleJ
    pressureI = particlePressure particleI envDensity densityI
    pressureJ = particlePressure particleJ envDensity densityJ
    
    absForce = (pressureI + pressureJ) / (2 * densityJ)
    dir = vectorDifference (position particleJ) (position particleI)
    
    forceVector = normalizeVector dir absForce

viscosityForceFunc :: [Particle] -> Ai
viscosityForceFunc particles particleI particleJ = forceVector
  where
    densityJ = particleDensity particles particleJ
    velocityI = vectorMagnitude (velocity particleI)
    velocityJ = vectorMagnitude (velocity particleJ)
    
    absForce = (velocityI - velocityJ) / densityJ
    -- TODO: implement viscosity dir
    dir = (0, 0)
    
    forceVector = normalizeVector dir absForce

tensionForceFunc :: [Particle] -> Ai
tensionForceFunc particles particleI particleJ = forceVector
  where
    densityJ = particleDensity particles particleJ
    
    absForce = 1 / densityJ
    dir = vectorDifference (position particleI) (position particleJ)
    
    forceVector = normalizeVector dir absForce

pressureForce :: [Particle]  -- Fluid
              -> Particle    -- Main particle
              -> Float       -- environment density
              -> Vector      -- resulting force vector
pressureForce particles particleI envDensity = forceVector
  where
    kernelFunc = pressureKernel (config particleI)
    forceVector = particleValue particles particleI kernelFunc (pressureForceFunc particles envDensity)

viscosityForce :: [Particle]  -- Fluid
               -> Particle    -- Main particle
               -> Vector      -- resulting force vector
viscosityForce particles particleI = vectorMul forceVector u
  where
    kernelFunc = viscosityKernel (config particleI)
    forceVector = particleValue particles particleI kernelFunc (viscosityForceFunc particles)
    u = viscosity (config particleI)

tensionForce :: [Particle]  -- Fluid
             -> Particle    -- Main particle
             -> Vector      -- resulting force vector
tensionForce particles particleI = vectorMul forceVector o
  where
    kernelFunc = tensionKernel (config particleI)
    forceVector = particleValue particles particleI kernelFunc (tensionForceFunc particles)
    o = surfaceTension (config particleI)

totalForce :: [Particle]  -- Fluid
           -> Particle    -- Main particle
           -> Float       -- environment density
           -> Vector      -- resulting force vector
totalForce particles particleI envDensity = vectorSum [pressureForce particles particleI envDensity,
                                                       viscosityForce particles particleI,
                                                       tensionForce particles particleI]


{-
pressureOfParticle :: [Particle] -> Particle -> Environment -> Float
pressureOfParticle pList p env = pStiffness * (pDensity - envDensity)
    where
        envDensity = densityOfEnvironment env
        pDensity = densityOfParticle pList p
        pStiffness = stiffness (config p)

-- Base function to calculate formula SumI(W(...)*m*(...))
baseSumFormula :: [Particle]
               -> Point
               -> ([Particle] -> Point -> Float) -- pass densityOfParticle by default
               -> (FluidConfig -> KernelFunc)
               -> (Particle -> Float) -- Ai
               -> Float
baseSumFormula pList pos densityF kernelType ai = result neighParticles resI
    where
        pSmoothingLength = smoothingLength (config (pList !! 0))
        neighParticles = findNeighbours pList pos pSmoothingLength

        rDiff :: Particle -> Vector
        rDiff pI = vectorDifference pos (position pI)

        resI :: Particle -> (FluidConfig -> KernelFunc) -> Float
        resI pI kernelType = (mass (config pI) * (ai pI) / (densityF pList pos))
                        * kernelFunc (vectorMagnitude (rDiff pI)) pSmoothingLength
            where
                kernelFunc = kernelType (config pI)

        result :: [Particle] -> (Particle -> (FluidConfig -> KernelFunc) -> Float) -> Float
        result (p : ps) pFunc = pFunc p kernelType + result ps pFunc
            where


densityAtPoint :: [Particle] -> Point -> Float
densityAtPoint pList pos = baseSumFormula pList pos densityF densityKernel ai
    where
        densityF = (\_ _ -> 1.0)
        ai = (\_ -> 1.0)

densityOfParticle :: [Particle] -> Particle -> Float
densityOfParticle pList p = densityAtPoint pList (position p)

pressureAtPoint :: [Particle] -> Point -> Environment -> Float
pressureAtPoint pList pos env = baseSumFormula pList pos densityAtPoint pressureKernel ai
    where
        ai pI = pressureOfParticle pList pI env

pressureForceAtPoint :: [Particle] -> Point -> Environment -> Float
pressureForceAtPoint pList pos env = baseSumFormula pList pos densityAtPoint pressureKernel ai
    where
        ai pI = (pressureAtPoint pList pos env + pressureOfParticle pList pI env) / 2
-}


gravityForceOfParticle :: Particle -> Environment -> Force
gravityForceOfParticle particle env = (scalar * x, scalar * y)
 where
  g = gravityAcceleration env 
  massParticle = (mass . config) particle
  scalar = g * massParticle
  directionGravity = directionOfGravity env 
  x = fst directionGravity
  y = snd directionGravity

