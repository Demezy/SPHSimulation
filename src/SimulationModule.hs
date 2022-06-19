{-# OPTIONS_GHC -fdefer-typed-holes -fshow-hole-constraints -funclutter-valid-hole-fits #-}

module SimulationModule where

import Objects
import Graphics.Gloss

-- TODO Check value type
type Ai = (Particle -> Point -> Float)

vectorDifference :: Point -> Point -> Vector
vectorDifference (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)

vectorMagnitude :: Vector -> Float
vectorMagnitude (x, y) = sqrt (x**2 + y**2)

distance :: Point -> Point -> Float
distance points = vectorMagnitude.vectorDifference points

-- TODO Implement
kernelFunction :: KernelFunc
kernelFunction = undefined

densityOfParticle :: [Particle] -> Particle -> Float
densityOfParticle pList p = overallSum neighParticles sumElemI
    where
        pPos = position p
        pSmoothingLength = smoothingLength (config p)
        neighParticles = findNeighbours pList pPos pSmoothingLength

        rDiff :: Particle -> Vector
        rDiff pI = vectorDifference pPos (position pI)

        sumElemI :: Particle -> Float
        sumElemI pI = mass (config pI) * kernelFunction (rDiff pI) pSmoothingLength

        overallSum :: [Particle] -> (Particle -> Float) -> Float
        overallSum (p : ps) pFunc = pFunc p + overallSum ps pFunc

        -- appliedList = map sumElemI neighParticles
        -- overallSum = foldl (+) 0 appliedList


findNeighbours :: [Particle] -> Point -> Float -> [Particle]
findNeighbours pList point h = filter filterFunc pList
  where
    filterFunc particle = distance point (position particle) <= h

valueAtPoint :: position -> smoothingLength -> KernelFunc -> Ai -> value
valueAtPoint = undefined

pressureOfParticle :: [Particle] -> Particle -> Environment -> Float
pressureOfParticle pList p env = pStiffness * (pDensity - envDensity)
    where
        envDensity = densityOfEnvironment env
        pDensity = densityOfParticle pList p
        pStiffness = stiffness (config p)


gravityForceOfParticle :: Particle -> Environment -> Force
gravityForceOfParticle = undefined
