module Lib
  ( glossExample,
  )
where

import Debug.Trace
import Text.Printf
import GHC.List (length)
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import InitUniverse
import Objects
import QuadTree
import RenderingOfUniverse
import SimulationModule
import TimeModule
import UsefulFunctions
import UserInteraction
import Prelude hiding (length)

solid :: Solid
solid =
  Solid
    { isMovable = True,
      shape = rectanglePath 10 10,
      renderFunction = rf
    }

rf :: Solid -> Picture
rf = color green . polygon . shape

glossExample :: IO ()
glossExample = play window background fps initialWorld renderWorld handleWorld updateWorld
  where
    window = FullScreen
    background = white
    fps = 30
    initialWorld = uni
    renderWorld world = renderUniverse world
    handleWorld event world = handleEvent event world
    updateWorld dt world =  simulation dt world

-- Simulation -----------------------------------------------------------------
simulation :: Float -> Universe -> Universe
simulation dt universe =
  universe
    { fluid = particlesNew,
      fluidAsTree = newTree
    }
  where
    -- Old or constant values
    particlesOld = fluid universe
    particlesAsTreeOld = fluidAsTree universe
    env = environment universe
    envDensity = densityOfEnvironment env
    density = densityOfEnvironment env
    time = dt * timeMultiplier (environment universe)
    -- New values
    particlesNew' = map (particleDensity' . applyForces' . applyVelocity' . editParticleForcesConfig) particlesOld
    particlesNew = deleteOutOfBounds particlesNew'
    newTree = getParticleTree particlesNew
    -- Changing particles
    applyVelocity' p = applyVelocity p time (walls universe)
    applyForces' p = applyForce p (totalForce particlesAsTreeOld p env) time

    editParticleForcesConfig p = p
      { pf = pf' p,
        vf = vf' p,
        tf = tf' p,
        ff = ff' p,
        gf = gf' p
      }

    pf' p = pressureForce (oldNeighbours p) p envDensity
    vf' p = viscosityForce (oldNeighbours p) p
    tf' p = tensionForce (oldNeighbours p) p
    ff' p = frictionForce p
    gf' p = gravityForceOfParticle p env

    particleDensity' p = particleDensity (oldNeighbours p) p
    -- Helpful functions
    oldNeighbours p = findNeighbours particlesAsTreeOld (position p) (smoothingLength (config p))
