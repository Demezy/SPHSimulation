module TotalConfig (TotalConfig (..), UserInteractionConfig (..), ourProgramConfig) where

data UserInteractionConfig = UserInteractionConfig
  { timeStep :: Float
  }

defaultUserInteractionCfg =
  UserInteractionConfig
    { timeStep = 10
    }

data TotalConfig = TotalConfig
  { showForces :: Bool,
    forcesScalarVal :: Float,
    meshRenderMode :: Bool,
    circleRenderMode :: Bool,
    displaySmoothingLength :: Bool,
    displayVelocityVector :: Bool,
    velocityScalarVal :: Float,
    displayQuadTree :: Bool,
    userInteractionCfg :: UserInteractionConfig
  }

defaultProgramConfig :: TotalConfig
defaultProgramConfig =
  TotalConfig
    { showForces = False,
      forcesScalarVal = 1,
      meshRenderMode = True,
      circleRenderMode = False,
      displaySmoothingLength = False,
      displayVelocityVector = False,
      velocityScalarVal = 1,
      displayQuadTree = False,
      userInteractionCfg = defaultUserInteractionCfg
    }

debugConfig :: TotalConfig
debugConfig =
  defaultProgramConfig
    { showForces = True,
      forcesScalarVal = 10000 * 4,
      velocityScalarVal = 4,
      meshRenderMode = False,
      circleRenderMode = True,
      displayVelocityVector = True
    }

fullDebugConfig :: TotalConfig
fullDebugConfig =
  debugConfig
    { displaySmoothingLength = True,
      displayQuadTree = True
    }

fancyConfig :: TotalConfig
fancyConfig =
  defaultProgramConfig
    { meshRenderMode = True,
      circleRenderMode = True
    }

ourProgramConfig :: TotalConfig
ourProgramConfig = fullDebugConfig
