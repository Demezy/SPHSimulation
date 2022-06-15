module Objects
    ( Particle (Particle, position, velocity, mass)
    , FluidConfig (FluidConfig, stiffness, density, viscosity, tension,
                   densityKernel, pressureKernel, viscosityKernel, tensionKernel)
    , Fluid (Fluid, particles, config, smoothingLength, renderParticle)
    ) where

import Graphics.Gloss
  
data Particle = Particle
  { position :: Point
  , velocity :: Vector
  , mass :: Float
  }
  deriving (Eq)
  
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

data Fluid = Fluid
  { particles :: [Particle]
  , config :: FluidConfig
  , smoothingLength :: Float
  , renderParticle :: Particle -> Picture
  }
