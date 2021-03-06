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

data Particle = Particle
  { idx :: Index,
    pos :: Position,
    vel :: Velocity
  }

instance Eq Particle where
  particleA == particleB = idx particleA == idx particleB

type Particles = [Particle]

type Model = Particles

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
-- The initial model consists of one particle,
-- which will be drawn and updated according to `newton`.
--
-- This results in a particle moving from the center of the
-- screen to the right:
main1 :: IO ()
main1 = simulate windowDisplay white simulationRate initialModel drawingFunc updateFunc
  where
    initialModel :: Model
    initialModel = [Particle 1 (V2 0.0 0.0) (V2 1.0 0.0)]

    drawingFunc :: Model -> Picture
    drawingFunc = pictures . drawParticles

    updateFunc :: ViewPort -> Float -> Model -> Model
    updateFunc _ dt = newton dt

--

-- * Helper functions

-- |

-- |
-- Convert a list of `Particle` (`Particles`) into
-- a list of `Picture`
drawParticles :: Particles -> [Picture]
drawParticles = fmap drawParticle

-- |
-- Visualize a single particle as blue dot
drawParticle :: Particle -> Picture
drawParticle (Particle _ (V2 x y) _) = translate x' y' $ color (circleSolid dotSize)
  where
    x' = toPixels x
    y' = toPixels y
    color = Color (withAlpha 0.8 blue)

-- |
-- Update velocity of one particle
-- assuming no acceleration
newton :: Float -> Particles -> Particles
newton dt [Particle idx pos vel] = [Particle idx pos' vel]
  where
    pos' = pos + vel ^* dt
