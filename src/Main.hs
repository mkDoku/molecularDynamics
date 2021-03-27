module Main where

import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort
import           Linear.V2
import           Linear.Vector
import           System.Random

-- |
-- Select the main function of your choice:
-- mainNewton, mainNewton', ...
main :: IO ()
main = mainNewton'

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
-- Size of the `Particle`s
dotSize = 0.1

-- |
-- Window display config
--
-- Show a 800x800 window at the window coordinate (200, 800) with the title "MD in
-- Haskell"
windowDisplay :: Display
windowDisplay = InWindow "MD in Haskell" (800, 800) (200, 800)

-- |
-- Number of frames per second
simulationRate :: Int
simulationRate = 60

-- |
-- Transform a value to the scale of Pixels
toPixels :: Float -> Float
toPixels = (* 100.0)

-- |
-- Length in the x dimension (width)
aLength = 7

-- |
-- Length in the y dimension (height)
bLength = 7

--

-- * Simulations

-- |

--

-- ** First simulation

--

-- |
-- The initial model consists of one `Particle`,
-- which will be drawn and updated according to `newton`.
--
-- This results in a `Particle` moving from the center of the
-- screen to the right:
mainNewton :: IO ()
mainNewton = simulate windowDisplay white simulationRate initialModel drawingFunc updateFunc
  where
    initialModel :: Model
    initialModel = [Particle 1 (V2 0.0 0.0) (V2 1.0 0.0)]

    drawingFunc :: Model -> Picture
    drawingFunc = pictures . drawParticles

    updateFunc :: ViewPort -> Float -> Model -> Model
    updateFunc _ dt = newton dt

--

-- *** Helper functions

-- |

-- |
-- Convert a list of `Particle` (`Particles`) into
-- a list of `Picture`
drawParticles :: Particles -> [Picture]
drawParticles = fmap drawParticle

-- |
-- Visualize a single `Particle` as blue dot
drawParticle :: Particle -> Picture
drawParticle (Particle _ (V2 x y) _) = translate x' y' $ color (circleSolid $ toPixels dotSize)
  where
    x' = toPixels x
    y' = toPixels y
    color = Color (withAlpha 0.8 blue)

-- |
-- Update velocity of one `Particle`
-- assuming no acceleration
newton :: Float -> Particles -> Particles
newton dt [Particle idx pos vel] = [Particle idx pos' vel]
  where
    pos' = pos + vel ^* dt

-- |

--

-- ** Second simulation

--

-- |
-- The initial model consists of one `Particle`,
-- which will be drawn and updated according to `newtonBounce`.
--
-- This results in a `Particle` moving from the center of the
-- screen to the right and will bounce of the wall:
mainNewton' :: IO ()
mainNewton' = simulate windowDisplay white simulationRate initialModel drawingFunc updateFunc
  where
    initialModel :: Model
    initialModel = [Particle 1 (V2 0.0 0.0) (V2 1.0 0.0)]

    drawingFunc :: Model -> Picture
    drawingFunc = pictures . (:) drawWalls . drawParticles

    updateFunc :: ViewPort -> Float -> Model -> Model
    updateFunc _ dt = newtonBounce dt

-- |

-- *** Additional helper functions

-- |

-- |
-- A `Picture` of the wall surrounding the `Particle` ("simulation box")
drawWalls :: Picture
drawWalls = lineLoop $ rectanglePath (toPixels aLength) (toPixels bLength)

-- |
-- Updated `newton` function, which incorporates bouncing of the wall (see
-- `boundaryCondition`)
newtonBounce :: Float -> Particles -> Particles
newtonBounce dt [particle@(Particle idx pos vel)] = [Particle idx pos' vel']
  where
    transVec = boundaryCondition particle
    vel' = transVec * vel
    pos' = pos + vel' ^* dt

-- |
-- Transformation vector, which is used to change the direction of a `Particle`, when it
-- is close (current position + radius of the particle) to a wall in x,
-- y or (x+y) direction.
boundaryCondition :: Particle -> V2 Float
boundaryCondition (Particle _ (V2 x y) _)
  | (x' > aLength/2) && (y' > bLength/2) = V2 (-1) (-1)
  |  x' > aLength/2                      = V2 (-1)   1
  |  y' > bLength/2                      = V2   1  (-1)
  | otherwise                            = V2   1    1
   where
     x' = abs x + dotSize
     y' = abs y + dotSize
