module InitUniverse where

import Objects
import SampleUnits
import UsefulFunctions

uni :: Universe
uni =
  defaultUniverse
    { fluid = sampleGridParticles ++ sampleParticles,
      fluidAsTree = getParticleTree (sampleGridParticles ++ sampleParticles),
      walls =
        [ wall1 ((750, 400), (500, -400)),
          wall1 ((500, -400), (0.1, -50)),
          wall1 ((0.1, -50), (-500, -400)),
          wall1 ((-500, -400), (-750, 400)),
          wall1 ((-750, 400), (750, 400))
        ]
    }

sampleGridParticles = [sampleParticle {position = (x * mult, y * mult)} | x <- [-d .. d], y <- [-d .. d]]
  where
    n = 0
    mult = 50
    d = sqrt (fromIntegral n) / 2

sampleParticles =
  map
    ( \x ->
        sampleParticle
          { position = (sin (angle x) * r, cos (angle x) * (r / 2) + y_pos),
            velocity = (0, 0)
          }
    )
    [1 .. n]
  where
    n = 50
    angle x = (2 * pi * x) / n
    r = 150
    y_pos = 50
