module TimeModule where

import Objects
import UsefulFunctions
import Graphics.Gloss (Point, Vector)

-- | Find all collisions
-- getCollisions :: particle -> walls
getCollisions :: Particle -> [Wall] -> [(Wall, Point, Float)]
getCollisions _ [] = []
getCollisions particle (w:ws)
  | collisionDistance <= r = collision : getCollisions particle ws
  | otherwise = getCollisions particle ws
  where
    point = position particle
    r = radius particle

    nearestPoint = segmentNearestPoint point (pos w)
    collisionDistance = distance point nearestPoint

    collision = (w, nearestPoint, collisionDistance)

-- | Apply one collision to particle
-- applyCollision :: particle -> collision
applyCollision :: Particle -> (Wall, Point, Float) -> Particle
applyCollision particle (wall, (cx, cy), collisionDistance) = particle {position = new_pos, velocity = new_v}
  where
    r = radius particle
    v = velocity particle
    (p1, p2) = pos wall
    
    -- Apply shifts
    (px, py) = normalizeVector (vectorDiff (cx, cy) (position particle)) r
    new_pos = (cx + px, cy + py)
    
    -- Change velocity if angle is proper
    wall_vector = vectorDiff p1 p2
    new_v = if sameDirection (px, py) v then v else vectorProjection wall_vector v

-- | Apply collisions to particle:
-- Apply shifts and change velocity
-- applyCollisions :: particle -> collisions
applyCollisions :: Particle -> [(Wall, Point, Float)] -> Particle
applyCollisions particle [] = particle
applyCollisions particle (c:cs) = applyCollision (applyCollisions particle cs) c

applyVelocity :: Particle -> Float -> [Wall] -> Particle
applyVelocity p dt walls = new_p {position = (x', y')}
  where
    collisions = getCollisions p walls
    new_p = applyCollisions p collisions
    
    (x, y) = position new_p
    (dx, dy) = velocity new_p
    (x', y') = (x + dx * dt, y + dy * dt)

applyForce :: Particle -> Force -> Float -> Particle
applyForce particle force time =
  particle
    { velocity = (newVX, newVY)
--      force = force
    }
  where
    newVX = deltaVX + oldVX
    newVY = deltaVY + oldVY
    deltaVX = time * (forceX / massParticle)
    deltaVY = time * (forceY / massParticle)
    (forceX, forceY) = force
    (oldVX, oldVY) = velocity particle
    massParticle = (mass . config) particle
