module Lib
    ( glossExample
    ) where

import Graphics.Gloss.Interface.Pure.Game
import RenderingOfUniverse
import Graphics.Gloss
import Objects
import TimeModule
import SimulationModule

uni :: Universe
uni = Universe
  { simulationScale = (0.001,0.001)
  , environment     = env
  , fluid           = sampleParticles
  , walls           = []
  }

wall :: Solid
wall = Solid
  { isMovable      = True
  , shape          = rectanglePath 10 10
  , renderFunction = rf
  }

rf :: Solid -> Picture
rf = color green . polygon . shape

env :: Environment
env = Environment
  { timeMultiplier       = 10000
  , directionOfGravity   = (0, -1)
  , gravityAcceleration  = 1/1000000
  , densityOfEnvironment = 1
  }

sampleParticles = map (\x -> sampleParticle {position = (2*x**2, x), velocity = (0, 0)}) [1.. 20]

sampleParticle :: Particle
sampleParticle = Particle
  { position   = (0, 0)
  , velocity   = (0, 0)
  , config     = conf1
  }

sampleParticle2 :: Particle
sampleParticle2 = Particle
  { position    = (1, 0)
  , velocity    = (0, 0)
  , config      = conf2
  }

conf1 :: FluidConfig
conf1 = FluidConfig
  { coloring        = black
  , stiffness       = 1
  , smoothingLength = 10000
  , mass            = 1
  , viscosity       = 1
  , surfaceTension  = 1
  , densityKernel   = kernelFunctionIncompressible
  , pressureKernel  = kernelFunction1
  , viscosityKernel = kernelFunction2
  , tensionKernel   = kernelFunction2
  }

conf2 :: FluidConfig
conf2 = FluidConfig
  { coloring        = red
  , stiffness       = 1
  , smoothingLength = 10000
  , mass            = 1
  , viscosity       = 0
  , surfaceTension  = 0
  , densityKernel   = kernelFunction0
  , pressureKernel  = kernelFunction1
  , viscosityKernel = kernelFunction2
  , tensionKernel   = kernelFunction0
  }

glossExample :: IO ()
glossExample = play window background fps initialWorld renderWorld handleWorld updateWorld
 where
        window                  = FullScreen
        background              = blue
        fps                     = 60
        initialWorld            = uni
        renderWorld       world = renderUniverse world
        handleWorld event world = handleEvent event world
        updateWorld dt    world = simulation dt world

-- Simulation -----------------------------------------------------------------
simulation :: Float -> Universe -> Universe
simulation dt universe = universe{fluid = particlesNew}
  where
    time = dt* (timeMultiplier (environment universe))
    particlesOld = fluid universe
    env = environment universe
    density = densityOfEnvironment env
    particlesNew = map (applyVelocity' . applyForces') particlesOld
    applyVelocity' p = applyVelocity p time
    -- applyForces' p = applyForce p (_totalForces p) time
    applyForces' p = applyForce p (totalForce particlesOld p env) time
    -- applyForces' p = applyForce p (gravityForceOfParticle p env ) time


-- Events ---------------------------------------------------------------------
handleEvent :: Event -> Universe -> Universe
handleEvent event universe = universe

