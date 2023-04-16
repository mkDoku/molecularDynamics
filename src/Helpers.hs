module Helpers where

import qualified Graphics.Gloss as  GG
import           Graphics.Gloss.Data.ViewPort
import           Linear.Metric
import           Linear.V2
import           Linear.Vector
import           System.Random

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
-- Represents the acceleration vector \(\vec{a}\) as a two-dimensional vector
type Acceleration = V2 Float

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
-- Show a 800x800 window at the window coordinate (0, 0) with the title "MD in
-- Haskell"
windowDisplay :: GG.Display
windowDisplay = GG.InWindow "MD in Haskell" (800, 800) (0, 0)

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

-- *** Helper functions

-- |

-- |
-- Visualize a single `Particle` as blue dot
-- by transforming the `Position` of the `Particle` into
-- a `GG.Picture`, which can be rendered
drawParticle :: Particle -> GG.Picture
drawParticle (Particle _ (V2 x y) _) =
  GG.translate x' y' $ color (GG.circleSolid $ toPixels dotSize)
  where
    x' = toPixels x
    y' = toPixels y
    color = GG.Color (GG.withAlpha 0.8 GG.blue)

-- |

-- *** Additional helper functions

-- |

-- |
-- A `GG.Picture` of the wall surrounding the `Particle` ("simulation box")
drawWalls :: GG.Picture
drawWalls = GG.lineLoop $ GG.rectanglePath (toPixels aLength) (toPixels bLength)

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
mainVerlet :: IO ()
mainVerlet = GG.simulate windowDisplay GG.white simulationRate initialModel drawingFunc updateFunc
  where
    initialModel :: Model
    initialModel = [ Particle 1 (V2   0.3  0.0) (V2   0.0  0.0)
                   , Particle 2 (V2 (-0.3) 0.0) (V2 (-0.0) 0.0) ]

    drawingFunc :: Model -> GG.Picture
    drawingFunc = GG.pictures . (:) drawWalls . fmap drawParticle

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
m = 18

-- |
-- The \(\epsilon\) value of the [Lennard-Jones
-- potential](https://en.wikipedia.org/wiki/Lennard-Jones_potential)
epsilon :: Float
-- epsilon = 1.65
epsilon = 12.57

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
-- Repulsion term of the [Lennard-Jones
-- potential](https://en.wikipedia.org/wiki/Lennard-Jones_potential)
repulsion :: Position -> Position -> Force
repulsion posA posB = (epsilon * 48.0 * sigma12 / divisor ) *^ r
  where
    divisor = (norm r)^14
    r = posB ^-^ posA

-- |
-- Attraction term of the [Lennard-Jones
-- potential](https://en.wikipedia.org/wiki/Lennard-Jones_potential)
attraction :: Position -> Position -> Force
attraction posA posB = (epsilon * 24.0 * sigma6 / divisor ) *^ r
  where
    divisor = (norm r)^8
    r = posB ^-^ posA

-- |
-- Update the position/velocity of __one__ `Particle`
updatePosition, updateVelocity :: TimeStep -> Particle -> Acceleration -> Particle
updatePosition dt (Particle idx pos vel) acc = Particle idx newPos vel
  where
   newPos  = pos ^+^ velPart ^+^ accPart
   velPart = vel ^* dt
   accPart = acc ^* (0.5 * dt**2)

updateVelocity dt particle acc = Particle idx pos vel'
  where
    (Particle idx pos vel) = particle
    transVec = boundaryCondition particle
    vel' = transVec * (vel + (0.5 * dt) *^ acc)

-- |
-- Apply `updatePosition`/`updateVelocity` to
-- __all__ (list/set) `Particle`s
updatePositions, updateVelocities :: TimeStep -> [Particle] -> [Acceleration] -> [Particle]
updatePositions  dt = zipWith (updatePosition dt)
updateVelocities dt = zipWith (updateVelocity dt)



-- |

--

-- ** Fourth simulation

--

-- |
-- Same as `mainVerlet` but with a square lattice of \(4 \times 4\) `Particle`s
mainVerletSquare :: IO ()
mainVerletSquare = GG.simulate windowDisplay GG.white simulationRate initialModel drawingFunc updateFunc
  where
    initialModel :: Model
    initialModel = squareLatticeModel 4

    drawingFunc :: Model -> GG.Picture
    drawingFunc = GG.pictures . (:) drawWalls . fmap drawParticle

    updateFunc :: ViewPort -> Float -> Model -> Model
    updateFunc _ dt = verletStep dt

-- |

-- *** Additional helper functions

-- |

-- |
-- Function to construct a [square lattice](https://en.wikipedia.org/wiki/Square_lattice)
-- of dimension \(n \times n\)
squareLatticeModel :: Int -> [Particle]
squareLatticeModel n = zipWith3 Particle idxs poss vels
  where
    idxs = [1..(n^2)]
    poss = squareLattice n n
    vels = replicate (n^2) (V2 0.0 0.0)

-- |
-- Constructs the [square lattice](https://en.wikipedia.org/wiki/Square_lattice)
-- (`squareLatticeModel`) row by row.
-- Each row has the same distance from the next row
squareLattice :: Int -> Int -> [Position]
squareLattice _ 0   = []
squareLattice dim acc = latticeRow dim dim yPos ++ squareLattice dim (acc-1)
  where
    dy   = bLength / fromIntegral (dim+1)
    yPos = bLength/2 - (fromIntegral acc * dy)

-- |
-- Constructs a even-distanced row of `Particle`s
latticeRow :: Int -> Int -> Float -> [Position]
latticeRow _ 0 _ = []
latticeRow dim acc yPos = V2 xPos yPos : latticeRow dim (acc-1) yPos
  where
    dx   = aLength / fromIntegral (dim+1)
    xPos = aLength/2 - (fromIntegral acc * dx)

-- |

-- ** Fourth simulation

--

-- |
-- Same as `mainVerlet` but with 16 random generated `Particle`s
mainVerletRandom :: IO ()
mainVerletRandom = do
  seed <- newStdGen
  GG.simulate windowDisplay GG.white simulationRate (initialModel seed) drawingFunc updateFunc
    where
      initialModel :: RandomGen g => g -> Model
      initialModel = modelRandom 16

      drawingFunc :: Model -> GG.Picture
      drawingFunc = GG.pictures . (:) drawWalls . fmap drawParticle

      updateFunc :: ViewPort -> Float -> Model -> Model
      updateFunc _ dt = verletStep dt

-- |

-- *** Additional helper functions

-- |

-- |
-- Generates \(n\) random `Particle`s (random `Position` and `Velocity`).
modelRandom :: RandomGen g => Int -> g -> [Particle]
modelRandom n g = zipWith3 Particle idxs poss vels
  where
    idxs = [1..n]
    (g', g'') = split g
    poss = randomPos n g'
    vels = randomVel n g''

-- |
-- Generates \(n\) random `Velocity` values
--
-- __Range__: \(v \in [-0.2,0.2]\)
randomVel :: RandomGen g => Int -> g -> [Velocity]
randomVel n g = take n $ randomRs ( -0.2, 0.2 ) g :: [Velocity]

-- |
-- Generates \(n\) random `Position`s
randomPos :: RandomGen g => Int -> g -> [Position]
randomPos 0 g = []
randomPos n g = newPos:randomPos n' g'
  where
    (newPos, g') = genPos g
    n' = n - 1

-- |
-- Generates \(1\) random `Position`
--
-- __Range__: All values inside the simulation box ('drawWalls')
genPos :: RandomGen g => g -> (Position, g)
genPos g = (pos, g'')
  where
    (xGen,  g') = randomR ( -aLengthHalf, aLengthHalf ) g
    (yGen, g'') = randomR ( -bLengthHalf, bLengthHalf ) g'
    pos = V2 xGen yGen
    aLengthHalf = aLength / 2 - dotSize
    bLengthHalf = bLength / 2 - dotSize
