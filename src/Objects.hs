module Objects
    ( Universe (Universe, simulationScale, environment, fluid, walls)
    , Particle (Particle, position, velocity, config)
    , Solid (Solid, isMovable, shape, renderFunction)
    , Environment (Environment, timeMultiplier, directionOfGravity,
                   gravityAcceleration, densityOfEnvironment)
    , FluidConfig (FluidConfig, coloring, stiffness, smoothingLength, mass,
                   viscosity, surfaceTension, densityKernel, pressureKernel,
                   viscosityKernel, tensionKernel)
    ) where

import Graphics.Gloss

data Universe = Universe
  { simulationScale :: (Float, Float)
  , environment      :: Environment
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

data Environment = Environment
  { timeMultiplier      :: Float
  , directionOfGravity  :: Vector 
  , gravityAcceleration :: Float 
  , densityOfEnvironment :: Float
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
