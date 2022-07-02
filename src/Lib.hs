module Lib
    ( glossExample
    ) where

import Graphics.Gloss.Interface.Pure.Game
import RenderingOfUniverse
import Graphics.Gloss
import Objects
import TimeModule
import SimulationModule
import UserInteraction
import UsefulFunctions
import QuadTree

getParticleTree :: [Particle] -> QuadTree Particle
getParticleTree = getTree 4 ((-2000, -2000), (2000, 2000)) position

uni :: Universe
uni = Universe
  { simulationScale = (0.001,0.001)
  , environment     = env
  , fluid           = sampleGridParticles ++ sampleParticles
  , fluidAsTree     = getParticleTree (sampleGridParticles ++ sampleParticles)
  , walls           = [wall1 ((750, 400), (500, -400)),
                       wall1 ((500, -400), (0.1, -50)),
                       wall1 ((0.1, -50), (-500, -400)),
                       wall1 ((-500, -400), (-750, 400)),
                       wall1 ((-750, 400), (750, 400))]
  , marker = (0,0)
  }

wall1 :: (Point, Point) -> Wall
wall1 (p1, p2) = Wall (p1, p2) renderWallGreen
  where
    renderWallGreen _ = line [p1, p2]

solid :: Solid
solid = Solid
  { isMovable      = True
  , shape          = rectanglePath 10 10
  , renderFunction = rf
  }

rf :: Solid -> Picture
rf = color green . polygon . shape

env :: Environment
env = Environment
  { timeMultiplier       = 500
  , directionOfGravity   = (0, -1)
  , gravityAcceleration  = 1/10000
  , densityOfEnvironment = 1
  }

sampleGridParticles = [sampleParticle{position = (x*mult, y*mult)} | x <- [-d.. d], y <- [-d.. d]]
  where
    n = 0
    mult = 50
    d = sqrt (fromIntegral n) / 2
sampleParticles = map (\x -> sampleParticle {position = (sin (angle x) * r, cos (angle x) * (r / 2) + y_pos),
                                             velocity = (0, 0)}) [1.. n]
  where
    n = 50
    angle x = (2 * pi * x) / n
    r = 150
    y_pos = 50

sampleParticle :: Particle
sampleParticle = Particle
  { position   = (0, 0)
  , velocity   = (0, 0)
  , radius     = 10
  , config     = conf1
  }

sampleParticle2 :: Particle
sampleParticle2 = Particle
  { position    = (1, 0)
  , velocity    = (0, 0)
  , radius      = 10
  , config      = conf2
  }

conf1 :: FluidConfig
conf1 = FluidConfig
  { coloring        = black
  , stiffness       = 1
  , smoothingLength = 200
  , mass            = 1
  , viscosity       = 1
  , surfaceTension  = 1
  , friction        = 0
  , minSpeed        = 0
  , densityKernel   = kernelFunction0
  , pressureKernel  = kernelFunction1
  , viscosityKernel = kernelFunction2
  , tensionKernel   = kernelFunction2
  }

conf2 :: FluidConfig
conf2 = FluidConfig
  { coloring        = red
  , stiffness       = 1
  , smoothingLength = 200
  , mass            = 1
  , viscosity       = 0
  , surfaceTension  = 0
  , friction        = 1
  , minSpeed        = 1e-2
  , densityKernel   = kernelFunction0
  , pressureKernel  = kernelFunction1
  , viscosityKernel = kernelFunction2
  , tensionKernel   = kernelFunction0
  }

glossExample :: IO ()
glossExample = play window background fps initialWorld renderWorld handleWorld updateWorld
 where
        window                  = FullScreen
        background              = white
        fps                     = 30
        initialWorld            = uni
        renderWorld       world = renderUniverse world
        handleWorld event world = handleEvent event world
        updateWorld dt    world = simulation dt world

-- Simulation -----------------------------------------------------------------
simulation :: Float -> Universe -> Universe
simulation dt universe = universe{fluid = particlesNew,
                                 fluidAsTree = newTree}
  where
    time = dt* timeMultiplier (environment universe)
    particlesOld = fluid universe
    particlesAsTreeOld = fluidAsTree universe
    env = environment universe
    density = densityOfEnvironment env

    particlesNew = map (applyForces' . applyVelocity') particlesOld
    newTree = getParticleTree particlesNew
    applyVelocity' p = applyVelocity p time (walls universe)
    -- applyForces' p = applyForce p (_totalForces p) time
    envDensity = densityOfEnvironment env
    densityMap = getDensityMap (getDensityDict particlesOld) envDensity
    applyForces' p = applyForce p (totalForce particlesAsTreeOld densityMap  p env) time
    -- applyForces' p = applyForce p (gravityForceOfParticle p env ) time


