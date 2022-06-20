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
  { simulationScale = (1,1)
  , environment     = env
  , fluid           = [sampleParticle, sampleParticle2]
  , walls           = [wall] 
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
  { timeMultiplier       = 1
  , directionOfGravity   = (0, -1)
  , gravityAcceleration  = 1000
  , densityOfEnvironment = 1
  }

sampleParticle :: Particle
sampleParticle = Particle
  { position   = (0, 0)
  , velocity   = (100, 300)
  , config     = conf1
  }

sampleParticle2 :: Particle
sampleParticle2 = Particle
  { position    = (90, 0)
  , velocity    = (1, 500)
  , config      = conf2
  }

conf1 :: FluidConfig
conf1 = FluidConfig
  { coloring        = black
  , stiffness       = undefined
  , smoothingLength = undefined
  , mass            = 10
  , viscosity       = undefined
  , surfaceTension  = undefined
  , densityKernel   = undefined
  , pressureKernel  = undefined
  , viscosityKernel = undefined
  , tensionKernel   = undefined
  }

conf2 :: FluidConfig
conf2 = FluidConfig
  { coloring        = red
  , stiffness       = undefined
  , smoothingLength = undefined
  , mass            = 50 
  , viscosity       = undefined
  , surfaceTension  = undefined
  , densityKernel   = undefined
  , pressureKernel  = undefined
  , viscosityKernel = undefined
  , tensionKernel   = undefined
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
    particlesOld = fluid universe
    env = environment universe
    particlesNew = map (applyVelocity' . applyForces') particlesOld
    applyVelocity' p = applyVelocity p dt
    -- applyForces' p = applyForce p (_totalForces p) dt
    applyForces' p = applyForce p (gravityForceOfParticle p  env) dt


-- Events ---------------------------------------------------------------------
handleEvent :: Event -> Universe -> Universe
handleEvent event universe = universe

