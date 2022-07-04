module SampleUnits where

import Graphics.Gloss
import Objects
import QuadTree
import SimulationModule
import UsefulFunctions

defaultEnvironment :: Environment
defaultEnvironment =
  Environment
    { timeMultiplier = 50,
      directionOfGravity = (0, -1),
      gravityAcceleration = 0.02,
      densityOfEnvironment = 1
    }

defaultUniverse :: Universe
defaultUniverse =
  Universe
    { simulationScale = (0.001, 0.001),
      environment = defaultEnvironment,
      fluid = [],
      fluidAsTree = getParticleTree [],
      walls = [],
      marker = (-1000, -1000)
    }

sampleParticle :: Particle
sampleParticle =
  Particle
    { position = (0, 0),
      velocity = (0, 0),
      currentDensity = 0,
      radius = 10,
      config = conf1,
      pf = (0, 0),
      vf = (0, 0),
      tf = (0, 0),
      ff = (0, 0),
      gf = (0, 0)
    }

sampleParticle2 :: Particle
sampleParticle2 =
  Particle
    { position = (1, 0),
      velocity = (0, 0),
      currentDensity = 0,
      radius = 10,
      config = conf2
    }

conf1 :: FluidConfig
conf1 =
  FluidConfig
    { coloring = blue,
      stiffness = 1e-2,
      smoothingLength = 100,
      mass = 1e-2,
      viscosity = 1,
      surfaceTension = 3,
      friction = 0.03,
      minSpeed = 0,
      densityKernel = kernelFunction0,
      pressureKernel = kernelFunction1,
      viscosityKernel = kernelFunction2,
      tensionKernel = kernelFunction2
    }

conf2 :: FluidConfig
conf2 =
  FluidConfig
    { coloring = red,
      stiffness = 1,
      smoothingLength = 200,
      mass = 1,
      viscosity = 0,
      surfaceTension = 0,
      friction = 1,
      minSpeed = 1e-2,
      densityKernel = kernelFunction0,
      pressureKernel = kernelFunction1,
      viscosityKernel = kernelFunction2,
      tensionKernel = kernelFunction0
    }

wall1 :: (Point, Point) -> Wall
wall1 (p1, p2) = Wall (p1, p2) renderWallGreen
  where
    renderWallGreen _ = line [p1, p2]
