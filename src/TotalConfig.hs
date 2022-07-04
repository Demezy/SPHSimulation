module TotalConfig (TotalConfig (..), ourProgramConfig) where

data TotalConfig = TotalConfig
  { showForces :: Bool,
    forcesScalarVal :: Float,
    meshRenderMode :: Bool,
    circleRenderMode :: Bool,
    displaySmoothingLength :: Bool
  }

defaultProgramConfig :: TotalConfig
defaultProgramConfig =
  TotalConfig
    { showForces = False,
      forcesScalarVal = 1,
      meshRenderMode = True,
      circleRenderMode = False,
      displaySmoothingLength = False
    }

debugConfig :: TotalConfig
debugConfig =
  defaultProgramConfig
    { showForces = True,
      forcesScalarVal = 10000 * 2,
      meshRenderMode = False,
      circleRenderMode = True
    }

fullDebugConfig :: TotalConfig
fullDebugConfig =
  debugConfig
    { displaySmoothingLength = True
    }

fancyConfig :: TotalConfig
fancyConfig =
  defaultProgramConfig
    { meshRenderMode = True,
      circleRenderMode = True
    }

ourProgramConfig :: TotalConfig
ourProgramConfig = fancyConfig
