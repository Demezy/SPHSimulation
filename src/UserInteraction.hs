module UserInteraction where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Objects
import UsefulFunctions
import SampleUnits



-- ==== Game Logic ====

handleEvent :: Event -> Universe -> Universe
-- handle keyboard
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) universe = changeTimeMul (-1000) universe
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) universe = changeTimeMul 1000 universe
-- handle mouse
handleEvent (EventKey (MouseButton LeftButton) Down _ point) universe = addParticleToUniverse universe point
-- omit other Events
handleEvent _ universe = universe



-- ==== Implementation ====

-- TODO determine parameters for users particles.
-- For now use default one
createParticle :: Point -- ^ 
  -> Particle
createParticle point = sampleParticle {position = point}

addParticleToUniverse :: Universe -> Point -> Universe
addParticleToUniverse universe point = universe
  {
    fluid = createParticle point : fluid universe
  }
