module SituationGravity.Lib where

import Data.List (foldl')

import SituationGravity.Constants

newtype Position = Pos (Float, Float) deriving (Show, Eq)
newtype Velocity = Vel (Float, Float) deriving (Show, Eq)
newtype Acceleration = Accel (Float, Float) deriving (Show, Eq)

data Body = Body
  { position :: Position
  , velocity :: Velocity
  , mass :: Float
  } deriving (Show, Eq)

posX, posY :: Position -> Float
posX (Pos (x, _)) = x
posY (Pos (_, y)) = y

updatePosition :: Float -> Body -> Body
updatePosition dt body =
    let Pos (x, y) = position body
        Vel (vx, vy) = velocity body
    in body { position = Pos (x + vx * dt, y + vy * dt) }

updateVelocity :: Float -> (Acceleration, Body) -> Body
updateVelocity dt (Accel (ax, ay), body) =
    let Vel (vx, vy) = velocity body
    in body { velocity = Vel (vx + ax * dt, vy + ay * dt) }

foldBodies :: Body -> Acceleration -> Body -> Acceleration
foldBodies body (Accel (ax, ay)) body' =
    let Pos (bx, by)   = position body
        Pos (bx', by') = position body'
        (dx, dy) = (bx' - bx, by' - by)
        distSq = dx * dx + dy * dy
        f = (gravity * mass body') / (distSq * sqrt (distSq + softening))
    in Accel (ax + dx * f, ay + dy * f)

calcAcceleration :: [Body] -> Body -> (Acceleration, Body)
calcAcceleration bodies body =
    (foldl' (foldBodies body) (Accel (0, 0)) bodies, body)