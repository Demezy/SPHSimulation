module TotalConfig (TotalConfig (..), UserInteractionConfig(..), ourProgramConfig) where


data UserInteractionConfig = UserInteractionConfig {
    timeStep :: Float 
  }

defaultUserInteractionCfg = UserInteractionConfig {
  timeStep = 100
  }

data TotalConfig = TotalConfig
  { showForces :: Bool,
    forcesScalarVal :: Float,
    meshRenderMode :: Bool,
    circleRenderMode :: Bool,
    displaySmoothingLength :: Bool,
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
      userInteractionCfg =defaultUserInteractionCfg
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
