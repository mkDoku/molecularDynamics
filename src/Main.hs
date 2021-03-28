module Main where

import           Graphics.Gloss
import           Graphics.Gloss.Data.ViewPort
import           Linear.V2
import           Linear.Vector
import           System.Random

-- |
-- Select the main function of your choice
--
--    * 'mainNewton'
--    * 'mainNewtonBounce'
--    * 'mainVerlet1'
--
-- to perform the according simulation
main :: IO ()
main = mainVerlet1

-- * Types and data constructors

-- |
-- Represents the spatial vector (`Position`) \(\vec{r}\) as a two-dimensional vector
type Position = V2 Float

-- |
-- Represents the velocity vector \(\vec{v}\) as a two-dimensional vector
type Velocity = V2 Float

-- |
-- Index of a `Particle` to make it distinguishable from other `Particle`s
--
-- __Note__: `Position` and `Velocity` do not make a `Particle` distinguishable
-- and even if, floating point errors would make this comparison invalid
type Index = Int

-- |
-- Represents the force vector \(\vec{F}\) as a two-dimensional vector
type Force = V2 Float

-- |
-- Represents the time step in the simulation \( (\Delta t) \)
type TimeStep = Float

-- |
-- Representation of a `Particle`. All we need is a `Index` (comparison) and a `Position`
-- and `Velocity`. The mass of the particle `m` is assumed to be the same for all
-- `Particle`s
data Particle = Particle
  { idx :: Index,
    pos :: Position,
    vel :: Velocity
  }

instance Eq Particle where
  particleA == particleB = idx particleA == idx particleB

-- |
-- The `Model` consists of a set/list of `Particle`
type Model = [Particle]

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
-- Convert a list of `Particle` ([Particle]) into
-- a list of `Picture`
drawParticles :: [Particle] -> [Picture]
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
newton :: Float -> [Particle] -> [Particle]
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
mainNewtonBounce :: IO ()
mainNewtonBounce = simulate windowDisplay white simulationRate initialModel drawingFunc updateFunc
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
newtonBounce :: Float -> [Particle] -> [Particle]
newtonBounce dt [particle@(Particle idx pos vel)] = [Particle idx pos' vel']
  where
    transVec = boundaryCondition particle
    vel' = transVec * vel
    pos' = pos + vel' ^* dt

-- |
-- Transformation vector, which is used to change the direction of a `Particle`, when it
-- is close (current `Position` + radius of the particle) to a wall in x,
-- y or (x and y) direction.
boundaryCondition :: Particle -> V2 Float
boundaryCondition (Particle _ (V2 x y) _)
  | (x' > aLength/2) && (y' > bLength/2) = V2 (-1) (-1)
  |  x' > aLength/2                      = V2 (-1)   1
  |  y' > bLength/2                      = V2   1  (-1)
  | otherwise                            = V2   1    1
   where
     x' = abs x + dotSize
     y' = abs y + dotSize

-- |

--

-- ** Third simulation

--

-- |
-- First simulation using the [Velocity Verlet
-- algorithm](https://en.wikipedia.org/wiki/Verlet_integration#Velocity_Verlet)
-- and the [Lennard-Jones
-- potential](https://en.wikipedia.org/wiki/Lennard-Jones_potential)
-- of two `Particle`s. They attract and repulse each other.
mainVerlet1 :: IO ()
mainVerlet1 = simulate windowDisplay white simulationRate initialModel drawingFunc updateFunc
  where
    initialModel :: Model
    initialModel = [ Particle 1 (V2   0.3  0.0) (V2   0.0  0.0)
                   , Particle 2 (V2 (-0.3) 0.0) (V2 (-0.0) 0.0) ]

    drawingFunc :: Model -> Picture
    drawingFunc = pictures . (:) drawWalls . drawParticles

    updateFunc :: ViewPort -> Float -> Model -> Model
    updateFunc _ dt = verletStep dt

-- |

-- *** Additional helper functions

-- |

-- |
-- Implementation of the [Velocity Verlet
-- algorithm](https://en.wikipedia.org/wiki/Verlet_integration#Velocity_Verlet) (without
-- the half-step)
verletStep :: TimeStep -> Model -> Model
verletStep dt particles =
  let
     oldF     = calcForces particles
     oldA     = fmap (^/ m) oldF
     newPos   = updatePositions dt particles oldA
     newF     = calcForces newPos
     newA     = fmap (^/ m) newF
     addedF   = oldA ^+^ newA
     newParts = updateVelocities dt newPos addedF
  in newParts

-- |
-- Mass of all our `Particle`s
m :: Float
m = 40

-- |
-- The \(\epsilon\) value of the [Lennard-Jones
-- potential](https://en.wikipedia.org/wiki/Lennard-Jones_potential)
epsilon :: Float
epsilon = 1.65

-- |
-- The \(\sigma\) value of the [Lennard-Jones
-- potential](https://en.wikipedia.org/wiki/Lennard-Jones_potential)
sigma :: Float
sigma = 0.335

-- |
-- Helper function for
--
-- \(\sigma^6\)
sigma6 = sigma**6

-- |
-- Helper function for
--
-- \(\sigma^{12}\)
sigma12 = sigma**12

-- |
-- Calculate __all__ `Force`s on __all__ `Particle`s employing
-- 'calcForceAcc'
calcForces :: [Particle] -> [Force]
calcForces particles = calcForceAcc particles particles

-- |
-- Helper function for
-- calculating __all__ `Force`s on __all__ `Particle`s
calcForceAcc :: [Particle] -> [Particle] -> [Force]
calcForceAcc [particle] particles = calcForceOnOne particle particles
calcForceAcc (p:articles) particles =     calcForceOnOne p particles
                                      ^+^ calcForceAcc articles particles

-- |
-- Calculate __all__ `Force`s on __one__ `Particle`
calcForceOnOne :: Particle -> [Particle] -> [Force]
calcForceOnOne particle = fmap (calcForceBetween particle)

-- |
-- Calculates the `Force` between __two__ `Particle`
-- as long as they are not the same (compare `Index`)
calcForceBetween :: Particle -> Particle -> Force
calcForceBetween particleA particleB
  | particleA == particleB = V2 0.0 0.0
  | otherwise = rep - att
    where
      rep = repulsion  posA posB
      att = attraction posA posB
      posA = pos particleA
      posB = pos particleB

-- |
-- The [Euclidean norm](https://en.wikipedia.org/wiki/Norm\_(mathematics\)#Euclidean_norm)
-- for a two-dimensional vector
-- \[ ||\vec{r}|| = \sqrt{x \cdot x + y \cdot y} \]
norm :: V2 Float -> Float
norm (V2 x y) = sqrt $ x*x + y*y

-- |
-- Repulsion term of the [Lennard-Jones
-- potential](https://en.wikipedia.org/wiki/Lennard-Jones_potential)
repulsion :: Position -> Position -> Force
repulsion posA posB = r ^* (epsilon * 12.0 * sigma12 / divisor )
  where
    divisor = (norm r)^14
    r = posB ^-^ posA

-- |
-- Attraction term of the [Lennard-Jones
-- potential](https://en.wikipedia.org/wiki/Lennard-Jones_potential)
attraction :: Position -> Position -> Force
attraction posA posB = r ^* (epsilon * 6.0 * sigma6 / divisor )
  where
    divisor = (norm r)^8
    r = posB ^-^ posA

-- |
-- Update the position/velocity of __one__ `Particle`
updatePosition, updateVelocity :: TimeStep -> Particle -> Force -> Particle
updatePosition dt (Particle idx pos vel) force = Particle idx newPos vel
  where
   newPos  = pos   ^+^ velPart ^+^ accPart
   velPart = vel   ^* dt
   accPart = force ^* (0.5 * dt**2)

updateVelocity dt particle force = Particle idx pos vel'
  where
    (Particle idx pos vel) = particle
    transVec = boundaryCondition particle
    vel' = transVec * (vel + (0.5 * dt) *^ force)

-- |
-- Apply `updatePosition`/`updateVelocity` to
-- __all__ (list/set) `Particle`s
updatePositions, updateVelocities :: TimeStep -> [Particle] -> [Force] -> [Particle]
updatePositions  dt = zipWith (updatePosition dt)
updateVelocities dt = zipWith (updateVelocity dt)
