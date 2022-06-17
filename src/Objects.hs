module Objects
    ( Particle (Particle, position, velocity, mass, config)
    , FluidConfig (FluidConfig, stiffness, density, viscosity, tension,
                   densityKernel, pressureKernel, viscosityKernel, tensionKernel)
    , Fluid (Fluid, particles, smoothingLength)
    ) where

import Graphics.Gloss
  
data Particle = Particle
  { position :: Point
  , velocity :: Vector
  , mass :: Float
  , config :: FluidConfig
  }
  
type KernelFunc = Vector -> Float -> Float

data FluidConfig = FluidConfig
  { stiffness :: Float
  , density :: Float
  , viscosity :: Float
  , tension :: Float
  , densityKernel :: KernelFunc
  , pressureKernel :: KernelFunc
  , viscosityKernel :: KernelFunc
  , tensionKernel :: KernelFunc
  }

-- | Storage for all fluids (even of different types)
data Fluid = Fluid
  { particles :: [Particle]
  , smoothingLength :: Float
  }
