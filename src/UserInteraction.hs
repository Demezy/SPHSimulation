module UserInteraction where

import Objects
import UsefulFunctions
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game





handleEvent :: Event -> Universe -> Universe
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) universe = changeTimeMul (-1000) universe
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) universe = changeTimeMul 1000 universe
-- handleEvent (EventKey (MouseButton LeftButton) Down _ point) universe = universe 
handleEvent (EventKey (MouseButton LeftButton) Down _ point) universe = universe {marker = point}
handleEvent _ universe = universe
