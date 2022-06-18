module Objects
    ( Universe (Universe, simulationScale, enviroment, fluid, walls)
    , Particle (Particle, position, velocity, config)
    , Solid (Solid, isMovable, shape, renderFunction)
    , Enviroment (Enviroment, timeMultiplier, directionOfGravity,
                   gravityAcceleration, densityOfEnviroment)
    , FluidConfig (FluidConfig, coloring, stiffness, smoothingLength, mass,
                   viscosity, surfaceTension, densityKernel, pressureKernel,
                   viscosityKernel, tensionKernel)
    ) where

import Graphics.Gloss

data Universe = Universe
  { simulationScale :: (Float, Float)
  , enviroment      :: Enviroment
  , fluid           :: [Particle]
  , walls           :: [Solid]
  }

data Particle = Particle
  { position :: Point
  , velocity :: Vector
  , config   :: FluidConfig
  }

data Solid = Solid
  { isMovable      :: Bool
  , shape          :: Bool -- | placeholder 
  , renderFunction :: Solid -> Picture
  }

data Enviroment = Enviroment
  { timeMultiplier      :: Float
  , directionOfGravity  :: Vector 
  , gravityAcceleration :: Float 
  , densityOfEnviroment :: Float
  }

data FluidConfig = FluidConfig
  { coloring        :: Color
  , stiffness       :: Float
  , smoothingLength :: Float
  , mass            :: Float
  , viscosity       :: Float
  , surfaceTension  :: Float
  , densityKernel   :: KernelFunc
  , pressureKernel  :: KernelFunc
  , viscosityKernel :: KernelFunc
  , tensionKernel   :: KernelFunc
  }

type KernelFunc = Vector -> Float -> Float

type Force = Vector
