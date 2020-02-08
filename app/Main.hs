module Main where

import SituationGravity.Lib
import qualified SituationGravity.Constants as C

sun :: Body
sun = Body
    { position = Pos (-1.50324727873647e-6, -3.93762725944737e-6)
    , velocity = Vel (0, 0)
    , mass = 1
    }

earth :: Body
earth = Body
    { position = Pos (0.648778995445634, 0.747796691108466)
    , velocity = Vel (-4.85085525059392, 4.09601538682312)
    , mass = 3.0024584e-6
    }

step :: Body -> Body
step = updatePosition . updateVelocity . calcAcceleration [sun]

posToTSV :: Position -> String
posToTSV (Pos (x, y)) = show x ++ "\t" ++ show y

run :: Int -> String
run n = unlines $ fmap (posToTSV . position) $ take n $ iterate step earth

main :: IO ()
main = putStrLn $ run 200
