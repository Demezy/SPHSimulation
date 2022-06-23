module Objects
    ( Environment (..)
    , FluidConfig (..)
    , Universe    (..)
    , Particle    (..)
    , Solid       (..)
    , KernelFunc
    , Force
    ) where

import Graphics.Gloss

data Universe = Universe
  { simulationScale :: (Float, Float)
  , environment     :: Environment
  , fluid           :: [Particle]
  , walls           :: [Solid]
  }

data Particle = Particle
  { position :: Point
  , velocity :: Vector
  , config   :: FluidConfig
  }
instance Show Particle where
  show (Particle pos vel _) = "Position: " ++ show pos ++ "   |   " ++ "Velocity: " ++ show vel

data Solid = Solid
  { isMovable      :: Bool
  , shape          :: Path -- | placeholder 
  , renderFunction :: Solid -> Picture
  }

data Environment = Environment
  { timeMultiplier       :: Float
  , directionOfGravity   :: Vector 
  , gravityAcceleration  :: Float 
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

type KernelFunc = Float -> Float -> Float

type Force = Vector
