module Verlet where

import           Helpers

import qualified Graphics.Gloss as  GG
import           Graphics.Gloss.Data.ViewPort
import           Linear.Metric
import           Linear.V2
import           Linear.Vector
import           System.Random

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

-- ** Fifth simulation

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
