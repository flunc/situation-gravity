module SituationGravity.Lib where

import Data.List (foldl')

import qualified SituationGravity.Constants as C

newtype Position = Pos (Double, Double) deriving (Show, Eq)
newtype Velocity = Vel (Double, Double) deriving (Show, Eq)
newtype Acceleration = Accel (Double, Double) deriving (Show, Eq)

data Body = Body
  { position :: Position
  , velocity :: Velocity
  , mass :: Double
  } deriving (Show, Eq)

updatePosition :: Body -> Body
updatePosition body =
    let Pos (x, y) = position body
        Vel (vx, vy) = velocity body
    in body { position = Pos (x + vx * C.dt, y + vy * C.dt) }

updateVelocity :: (Acceleration, Body) -> Body
updateVelocity (Accel (ax, ay), body) =
    let Vel (vx, vy) = velocity body
    in body { velocity = Vel (vx + ax * C.dt, vy + ay * C.dt) }

foldBodies :: Body -> Acceleration -> Body -> Acceleration
foldBodies body (Accel (ax, ay)) body' =
    let Pos (bx, by)   = position body
        Pos (bx', by') = position body'
        (dx, dy) = (bx' - bx, by' - by)
        distSq = dx * dx + dy * dy
        f = (C.gravity * mass body') / (distSq * sqrt (distSq + C.softening))
    in Accel (ax + dx * f, ay + dy * f)

calcAcceleration :: [Body] -> Body -> (Acceleration, Body)
calcAcceleration bodies body =
    (foldl' (foldBodies body) (Accel (0, 0)) bodies, body)