module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Simulate

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

---

posToTSV :: Position -> String
posToTSV (Pos (x, y)) = show x ++ "\t" ++ show y

toTSV :: Int -> String
toTSV n = unlines $ fmap (posToTSV . position) $ take n $ iterate step earth
    where step = updatePosition C.dt . updateVelocity C.dt . calcAcceleration [sun]

runTSV :: IO ()
runTSV = putStrLn $ toTSV 100

---

data BBox a = BBox
    { left   :: a
    , top    :: a
    , right  :: a
    , bottom :: a
    }

bboxIntToFloat :: BBox Int -> BBox Float
bboxIntToFloat (BBox l t r b) = BBox (fromIntegral l) (fromIntegral t) (fromIntegral r) (fromIntegral b)

topLeft, bottomRight :: BBox a -> (a, a)
topLeft b     = (left b, top b)
bottomRight b = (right b, bottom b)

sceneBounds :: BBox Float
sceneBounds = BBox (-5.0) (-5.0) 5.0 5.0

displayBounds :: BBox Int
displayBounds = BBox 100 100 500 500

bboxWidth :: Num a => BBox a -> a
bboxWidth (BBox l _ r _) = r - l

bboxHeight :: Num a => BBox a -> a
bboxHeight (BBox _ t _ b) = b - t

roundTuple :: (Float, Float) -> (Int, Int)
roundTuple (x, y) = (round x, round y)

displayMode :: Display
displayMode = InWindow "Situation Gravity"
    (bboxWidth displayBounds, bboxHeight displayBounds)
    (left displayBounds, top displayBounds)

scalePosition :: BBox Float -> BBox Float -> Position -> Position
scalePosition displayBB sceneBB (Pos (x, y)) =
    let x' = bboxWidth displayBB * (x - (left sceneBB)) / bboxWidth sceneBB
        y' = bboxHeight displayBB * (y - (top sceneBB)) / bboxHeight sceneBB
    in Pos (x', y')

scaler :: Position -> Position
scaler = scalePosition (bboxIntToFloat displayBounds) sceneBounds

backgroundColour :: Color
backgroundColour = black

scaledSun :: Position
scaledSun = scaler $ position sun

runSimulate :: IO ()
runSimulate = simulate displayMode backgroundColour 30 earth draw step
    where
        draw :: Body -> Picture
        draw body =
            let scaledPos = scaler $ position body
            in pictures
                [ translate (posX scaledPos) (posY scaledPos) $ color blue $ circleSolid 4
                , translate (posX scaledSun) (posY scaledSun) $ color yellow $ circleSolid 10
                ]

        step :: ViewPort -> Float -> Body -> Body
        step _ dt = updatePosition dt . updateVelocity dt . calcAcceleration [sun]


main :: IO ()
main = runSimulate
