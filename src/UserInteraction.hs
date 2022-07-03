module UserInteraction where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Objects
import UsefulFunctions
import SampleUnits
import InitUniverse (uni)



-- ==== Game Logic ====

handleEvent :: Event -> Universe -> Universe
-- handle keyboard
handleEvent (EventKey (SpecialKey KeyDown) Down _ _) universe = changeTimeMul (-100) universe
handleEvent (EventKey (SpecialKey KeyUp) Down _ _) universe = changeTimeMul 100 universe
handleEvent (EventKey (SpecialKey KeySpace) Down _ _) universe = universe{
  environment = env{
    timeMultiplier = 0
    }
  }
  where 
    env = environment universe

handleEvent (EventKey (Char 'r') Down _ _) _ = uni

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
startDrawWall uni point = uni{
  marker = point
  }

stopDrawWall :: Universe -> Point -> Universe
stopDrawWall uni point = uni{
  walls = newWall : walls uni,
  marker = hideCursor (marker uni)
  }
  where
    newWall = wall1 (begin, end)
    begin = marker uni
    end = point
-- = hide mark cursor = --
hideCursor :: Cursor -> Cursor
hideCursor _ = (-1000, -1000)
