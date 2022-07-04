module InitUniverse where

import Objects
import SampleUnits
import UsefulFunctions

uni :: Universe
uni =
  defaultUniverse
    { fluid = sampleGridParticles ++ sampleParticles ++ imposterParticles,
      fluidAsTree = getParticleTree (sampleGridParticles ++ sampleParticles),
      walls =
        [
          wall1 ((750, 400), (500, -400)),
          wall1 ((500, -400), (0.1, -50)),
          wall1 ((0.1, -50), (-500, -400)),
          wall1 ((-500, -400), (-750, 400)),
          wall1 ((-750, 400), (750, 400))
        ]
    }

sampleGridParticles = [sampleParticle {position = (100 + x * mult, y * mult + 170)} | x <- [-d .. d], y <- [-d .. d]]
  where
    n = 50
    mult = 60
    d = sqrt (fromIntegral n) / 2

imposterParticles :: [Particle]
imposterParticles = [
  sampleParticle {position = (-dist, 0)},
  sampleParticle {position = (-dist, 1)}
  ]
  where
    dist = 400



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
    n = 0
    angle x = (2 * pi * x) / n
    r = 400
    y_pos = 75
