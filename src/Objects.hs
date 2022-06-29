module Objects
    ( Environment (..)
    , FluidConfig (..)
    , Universe    (..)
    , Particle    (..)
    , Wall       (..)
    , Solid (..)
    , KernelFunc
    , Force
    ) where

import Graphics.Gloss

data Universe = Universe
  { simulationScale :: (Float, Float)
  , environment     :: Environment
  , fluid           :: [Particle]
  , walls           :: [Wall]
  }

data Particle = Particle
  { position :: Point
  , velocity :: Vector
  , config   :: FluidConfig
  }
instance Show Particle where
  show (Particle pos vel _) = "Position: " ++ show pos ++ "   |   " ++ "Velocity: " ++ show vel
instance Eq Particle where
  (==) p1 p2 = position p1 == position p2

data Wall = Wall {pos :: (Point, Point), renderFunc :: Wall -> Picture}

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
  , smoothingLength :: Float
  , stiffness       :: Float
  , mass            :: Float
  , viscosity       :: Float
  , surfaceTension  :: Float
  , friction        :: Float
  , minSpeed        :: Float
  , densityKernel   :: KernelFunc
  , pressureKernel  :: KernelFunc
  , viscosityKernel :: KernelFunc
  , tensionKernel   :: KernelFunc
  }

type KernelFunc = Float -> Float -> Float

type Force = Vector
