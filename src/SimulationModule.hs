module SimulationModule where

import Graphics.Gloss
import Data.Maybe
import Objects
import UsefulFunctions

type Ai = (Particle -> Particle -> Vector)
type DensityMap = (Particle -> Float)

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

pressureForceFunc :: DensityMap -> Float -> Ai
pressureForceFunc densityMap envDensity particleI particleJ = forceVector
  where
    densityI = densityMap particleI
    densityJ = densityMap particleJ
    pressureI = particlePressure particleI envDensity densityI
    pressureJ = particlePressure particleJ envDensity densityJ
    
    absForce = (pressureI + pressureJ) / (2 * densityJ)
    dir = vectorDiff (position particleJ) (position particleI)
    
    forceVector = normalizeVector dir absForce

viscosityForceFunc :: DensityMap -> Ai
viscosityForceFunc densityMap particleI particleJ = forceVector
  where
    densityJ = densityMap particleJ

    multiplier = 1 / densityJ
    vector = vectorDiff (velocity particleI) (velocity particleJ)

    forceVector = vectorMul vector multiplier

tensionForceFunc :: DensityMap -> Ai
tensionForceFunc densityMap particleI particleJ = forceVector
  where
    densityJ = densityMap particleJ
    
    absForce = 1 / densityJ
    dir = vectorDiff (position particleJ) (position particleI)
    
    forceVector = normalizeVector dir absForce

pressureForce :: [Particle]  -- Fluid
              -> DensityMap  -- Density for each particle
              -> Particle    -- Main particle
              -> Float       -- environment density
              -> Vector      -- resulting force vector
pressureForce particles densityMap particleI envDensity = forceVector
  where
    kernelFunc = pressureKernel (config particleI)
    forceVector = particleValue particles particleI kernelFunc (pressureForceFunc densityMap envDensity)

viscosityForce :: [Particle]  -- Fluid
               -> DensityMap  -- Density for each particle
               -> Particle    -- Main particle
               -> Vector      -- resulting force vector
viscosityForce particles densityMap particleI = vectorMul forceVector u
  where
    kernelFunc = viscosityKernel (config particleI)
    forceVector = particleValue particles particleI kernelFunc (viscosityForceFunc densityMap)
    u = viscosity (config particleI)

tensionForce :: [Particle]  -- Fluid
             -> DensityMap  -- Density for each particle
             -> Particle    -- Main particle
             -> Vector      -- resulting force vector
tensionForce particles densityMap particleI = vectorMul forceVector o
  where
    kernelFunc = tensionKernel (config particleI)
    forceVector = particleValue particles particleI kernelFunc (tensionForceFunc densityMap)
    o = surfaceTension (config particleI)
    
-- | Particles -> envDensity -> [(particle, particleDensity)]
getDensityDict :: [Particle] -> [(Particle, Float)]
getDensityDict particles = map (\p -> (p, particleDensity particles p)) particles

-- | densityDict -> envDensity -> DensityMap
getDensityMap :: [(Particle, Float)] -> Float -> DensityMap
getDensityMap densityDict envDensity particle = density
  where
    foundDensity = lookup particle densityDict
    density = Data.Maybe.fromMaybe envDensity foundDensity

totalForce :: [Particle]  -- Fluid
           -> Particle    -- Main particle
           -> Environment -- environment density
           -> Vector      -- resulting force vector
totalForce particles particleI env = vectorSum [pressureForce particles densityMap particleI envDensity,
                                                --viscosityForce particles densityMap particleI,
                                                --tensionForce particles densityMap particleI,
                                                gravityForceOfParticle particleI env
                                                ]
  where
    envDensity =  densityOfEnvironment env
    densityMap = getDensityMap (getDensityDict particles) envDensity

gravityForceOfParticle :: Particle -> Environment -> Force
gravityForceOfParticle particle env = (scalar * x, scalar * y)
 where
  g = gravityAcceleration env 
  massParticle = (mass . config) particle
  scalar = g * massParticle
  directionGravity = directionOfGravity env 
  x = fst directionGravity
  y = snd directionGravity

