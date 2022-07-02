module Objects
    ( Environment (..)
    , FluidConfig (..)
    , Universe    (..)
    , Particle    (..)
    , Wall       (..)
    , Solid (..)
    , KernelFunc
    , Force
    , Collider (..)
    ) where

import Graphics.Gloss (Point, Vector, Path, Picture, Color)
import QuadTree (QuadTree)

data Universe = Universe
  { simulationScale :: (Float, Float)
  , environment     :: Environment
  , fluid           :: [Particle]
  , fluidAsTree     :: QuadTree Particle
  , walls           :: [Wall]
  , marker :: Point
  }

data Particle = Particle
  { position :: Point
  , velocity :: Vector
  , radius :: Float
  , config   :: FluidConfig
  }
instance Show Particle where
  show (Particle pos vel _ _) = "Position: " ++ show pos ++ "   |   " ++ "Velocity: " ++ show vel
instance Eq Particle where
  (==) p1 p2 = position p1 == position p2

data Wall = Wall {pos :: (Point, Point), renderFunc :: Wall -> Picture}

-- | CircleCollider :: centerPoint radius
--   SegmentCollider :: (point1, point2) perpendicularDirection
data Collider = CircleCollider Point Float | SegmentCollider (Point, Point) Vector deriving (Eq)

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
