module Main where

import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort
import           Linear.V2
import           Linear.Vector
import           System.Random

-- |
-- Select the main function of your choice:
-- main1, main2, ...
main :: IO ()
main = main1

-- * Types and data constructors

type Position = V2 Float

type Velocity = V2 Float

type Index = Int

type Force = V2 Float

type Forces = [Force]

type TimeStep = Float

data Ball = Ball
  { idx :: Index,
    pos :: Position,
    vel :: Velocity
  }

instance Eq Ball where
  ballA == ballB = idx ballA == idx ballB

type Balls = [Ball]

type Model = Balls

--

-- * Constant functions (throughout all main)

-- |
dotSize = 10

windowDisplay :: Display
windowDisplay = InWindow "MD in Haskell" (800, 800) (200, 800)

simulationRate :: Int
simulationRate = 60

toPixels :: Float -> Float
toPixels = (* 100.0)

--

-- * Simulations

-- |

--

-- ** First simulation"

--

-- |
-- The initial model consists of one ball,
-- which will be drawn and updated according to `newton`.
--
-- This results in a ball moving from the center of the
-- screen to the right:
main1 :: IO ()
main1 = simulate windowDisplay white simulationRate initialModel drawingFunc updateFunc
  where
    initialModel :: Model
    initialModel = [Ball 1 (V2 0.0 0.0) (V2 1.0 0.0)]

    drawingFunc :: Model -> Picture
    drawingFunc = pictures . drawBalls

    updateFunc :: ViewPort -> Float -> Model -> Model
    updateFunc _ dt = newton dt

--

-- * Helper functions

-- |

-- |
-- Convert a list of `Ball` (`Balls`) into
-- a list of `Picture`
drawBalls :: Balls -> [Picture]
drawBalls = fmap drawBall

-- |
-- Visualize a single ball as blue dot
drawBall :: Ball -> Picture
drawBall (Ball _ (V2 x y) _) = translate x' y' $ color (circleSolid dotSize)
  where
    x' = toPixels x
    y' = toPixels y
    color = Color (withAlpha 0.8 blue)

-- |
-- Update velocity of one ball
-- assuming no acceleration
newton :: Float -> Balls -> Balls
newton dt [Ball idx pos vel] = [Ball idx pos' vel]
  where
    pos' = pos + vel ^* dt
