module Lib
    ( glossExample
    ) where

import Graphics.Gloss
import Objects
import RenderingOfUniverse

window :: Display
window = InWindow "Nice Window" (1700, 1900) (10, 10)

background :: Color
background = blue

drawing :: Picture
drawing = circle 80

universe :: Universe
universe = Universe
  { simulationScale = (1,1)
  , environment      = env
  , fluid           = [sampleParticle, sampleParticle2]
  , walls           = undefined
  }

env :: Environment
env = Environment
  { timeMultiplier      = 1
  , directionOfGravity  = (1, 1)
  , gravityAcceleration = 1
  , densityOfEnvironment = 1
  }

sampleParticle :: Particle
sampleParticle = Particle
  { position   = (0, 0)
  , velocity   = (1, 1)
  , config     = conf1
  }

sampleParticle2 :: Particle
sampleParticle2 = Particle
  { position   = (90, 0)
  , velocity   = (1, 1)
  , config     = conf2
  }

conf1 :: FluidConfig
conf1 = FluidConfig
  { coloring        = black
  , stiffness       = undefined
  , smoothingLength = undefined
  , mass            = undefined
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
  , mass            = undefined
  , viscosity       = undefined
  , surfaceTension  = undefined
  , densityKernel   = undefined
  , pressureKernel  = undefined
  , viscosityKernel = undefined
  , tensionKernel   = undefined
  }

glossExample :: IO ()
glossExample = display window background (renderParticles universe) 
