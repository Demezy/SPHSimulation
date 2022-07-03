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
import InitUniverse




solid :: Solid
solid = Solid
  { isMovable      = True
  , shape          = rectanglePath 10 10
  , renderFunction = rf
  }

rf :: Solid -> Picture
rf = color green . polygon . shape


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


