module UserInteraction where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Objects
import UsefulFunctions
import SampleUnits
import Data.Fixed (Uni)



-- ==== Game Logic ====

handleEvent :: Event -> Universe -> Universe
-- handle keyboard
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) universe = changeTimeMul (-1000) universe
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) universe = changeTimeMul 1000 universe
-- handle mouse
-- prefer explicitly use universe as argument, so wouldn't omit it!
handleEvent (EventKey (MouseButton LeftButton) Down _ point) universe = addParticleToUniverse universe point
handleEvent (EventKey (MouseButton RightButton) Down _ point) universe = startDrawWall universe point
handleEvent (EventKey (MouseButton RightButton) Up _ point) universe = stopDrawWall universe point

-- omit other Events
handleEvent _ universe = universe



-- ==== Implementation ====

-- == Add particles by LMB ==
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



-- == draw walls == 
startDrawWall :: Universe -> Point -> Universe
startDrawWall u _ = u

stopDrawWall :: Universe -> Point -> Universe
stopDrawWall u _ = u
