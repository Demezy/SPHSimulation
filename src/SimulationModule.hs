module SimulationModule where

import Debug.Trace
import Data.Maybe
import Graphics.Gloss
import Objects
import QuadTree
import UsefulFunctions

type Ai = (Particle -> Particle -> Vector)

type DensityMap = (Particle -> Float)

kernelFunction0 :: KernelFunc
kernelFunction0 r h
  | 0 <= r && r <= h = (315 / (64 * pi * (h ** 9))) * (((h ** 2) - (r ** 2)) ** 3)
  | otherwise = 0

kernelFunction1 :: KernelFunc
kernelFunction1 r h
  | 0 <= r && r <= h = (-(315 / (64 * pi * (h ** 9)))) * (6 * r) * ((h ** 2) - (r ** 2)) ** 2
  | otherwise = 0

kernelFunction2 :: KernelFunc
kernelFunction2 r h
  | 0 <= r && r <= h = (315 / (64 * pi * (h ** 9))) * 6 * ((h ** 2) - (r ** 2)) * (4 * (r ** 2) - ((h ** 2) - (r ** 2)))
  | otherwise = 0

kernelFunctionIncompressible :: KernelFunc
kernelFunctionIncompressible r h
  | 0 <= r && r <= h = (1 - r / h) ** 2
  | otherwise = 0

findNeighbours :: QuadTree Particle -> Point -> Float -> [Particle]
findNeighbours tree = getObjectsInRadius tree position

-- filter filterFunc pList
-- where
--   filterFunc particle = (position particle /= point) && (distance point (position particle) <= h)

particleValue ::
  [Particle] -> -- Fluid
  Particle -> -- Main particle
  KernelFunc -> -- kernel function
  Ai -> -- function to calculate force between 2 particles
  Vector -- resulting force vector
particleValue neighbours particleI kernelFunc func
  | null neighbours = (0, 0)
  | otherwise = vectorSum (map itemResult neighbours)
  where
    h = smoothingLength (config particleI)
    -- neighbours = findNeighbours particles (position particleI) h

    itemResult particleJ = vectorMul itemForce (mass (config particleJ) * kernelFunc r h)
      where
        r = distance (position particleI) (position particleJ)
        itemForce = func particleI particleJ

-- particleDensity :: [Particle]  -- Fluid
--                 -> Particle    -- Main particle
--                 -> Float       -- resulting density
-- particleDensity particles particleI = fst (particleValue particles particleI kernelFunc (\_ _ -> (1, 0)))
--   where
--     kernelFunc = densityKernel (config particleI)
particleDensity ::
  [Particle] -> -- Fluid
  Particle -> -- Main particle
  Particle -- New particle with updated density
particleDensity particles particleI =
  particleI {currentDensity = sum (map densityWithOneParticlue particles)}
  where
    kernelFunc = densityKernel (config particleI)
    h = smoothingLength (config particleI)
    distanceTo p = distance (position particleI) (position p)
    densityWithOneParticlue p = mass (config p) * kernelFunc (distanceTo p) h

particlePressure ::
  Particle -> -- Main particle
  Float -> -- environment density
  Float -> -- density at main particle
  Float -- resulting pressure
particlePressure particleI envDensity densityI = k * (densityI - envDensity)
  where
    k = stiffness (config particleI)

checkStupidVector :: String -> Vector -> Vector
checkStupidVector msg vector = if ((isNaN x) || (isNaN y))
      then trace (msg ++ show vector) vector
      else vector
      where
        (x, y) = vector

pressureForceFunc ::  Float -> Ai
pressureForceFunc envDensity particleI particleJ
  | densityJ == 0 = (0, 0)
  | otherwise = forceVector
  where
    densityI = currentDensity particleI
    densityJ = currentDensity particleJ
    pressureI = particlePressure particleI envDensity densityI
    pressureJ = particlePressure particleJ envDensity densityJ

    absForce = (pressureI + pressureJ) / (2 * densityJ)
    dir = vectorDiff (position particleJ) (position particleI)

    forceVector = normalizeVector dir absForce
      


viscosityForceFunc ::  Ai
viscosityForceFunc particleI particleJ
  | densityJ == 0 = (0, 0)
  | otherwise = forceVector
  where
    densityJ = currentDensity particleJ

    multiplier = 1 / densityJ
    vector = vectorDiff (velocity particleI) (velocity particleJ)

    forceVector = vectorMul vector multiplier

tensionForceFunc ::  Ai
tensionForceFunc particleI particleJ
  | densityJ == 0 = (0, 0)
  | otherwise = forceVector
  where
    densityJ = currentDensity particleJ

    absForce = 1 / densityJ
    dir = vectorDiff (position particleI) (position particleJ)

    forceVector = normalizeVector dir absForce

-- O(n^2)???
pressureForce ::
  [Particle] -> -- Fluid
  Particle -> -- Main particle
  Float -> -- environment density
  Vector -- resulting force vector
pressureForce particles particleI envDensity = forceVector
  where
    kernelFunc = pressureKernel (config particleI)
    forceVector = particleValue particles particleI kernelFunc (pressureForceFunc envDensity)

viscosityForce ::
  [Particle] -> -- Fluid
  Particle -> -- Main particle
  Vector -- resulting force vector
viscosityForce particles particleI = vectorMul forceVector u
  where
    kernelFunc = viscosityKernel (config particleI)
    forceVector = particleValue particles particleI kernelFunc viscosityForceFunc
    u = viscosity (config particleI)

tensionForce ::
  [Particle] -> -- Fluid
  Particle -> -- Main particle
  Vector -- resulting force vector
tensionForce particles particleI = vectorMul forceVector o
  where
    kernelFunc = tensionKernel (config particleI)
    forceVector = particleValue particles particleI kernelFunc tensionForceFunc
    o = surfaceTension (config particleI)

frictionForce :: Particle -> Vector
frictionForce particle = forceVector
  where
    v = velocity particle
    m = mass (config particle)
    k = friction (config particle)
    forceVector = vectorMul v (m * (-k))

-- | Particles -> envDensity -> [(particle, particleDensity)]
-- getDensityDict :: [Particle] -> [(Particle, Float)]
-- getDensityDict particles = map (\p -> (p, particleDensity particles p)) particles

-- | densityDict -> envDensity -> DensityMap
-- getDensityMap :: [(Particle, Float)] -> Float -> DensityMap
-- getDensityMap densityDict envDensity particle = density
--   where
--     foundDensity = lookup particle densityDict
--     density = fromMaybe envDensity foundDensity
totalForce ::
  QuadTree Particle -> -- Fluid
  Particle -> -- Main particle
  Environment -> -- environment density
  Vector -- resulting force vector
totalForce tree particleI env = res
  where 
    res' = vectorSum
      [
        pressureForce particles particleI envDensity,
        viscosityForce particles particleI,
        tensionForce particles particleI,
        frictionForce particleI, -- good
        gravityForceOfParticle particleI env -- good
      ]
      where
        envDensity = densityOfEnvironment env
        particles = findNeighbours tree (position particleI) h
        h = smoothingLength (config particleI)
    res = checkStupidVector "TOTAL FFORCE" res'

-- O(n^2)
-- densityMap = getDensityMap (getDensityDict particles) envDensity

gravityForceOfParticle :: Particle -> Environment -> Force
gravityForceOfParticle particle env = (scalar * x, scalar * y)
  where
    g = gravityAcceleration env
    massParticle = (mass . config) particle
    scalar = g * massParticle
    directionGravity = directionOfGravity env
    x = fst directionGravity
    y = snd directionGravity
