module Newton ( mainNewton
              , mainNewtonBounce
              ) where

import Helpers

import qualified Graphics.Gloss as  GG
import           Graphics.Gloss.Data.ViewPort
import           Linear.Metric
import           Linear.V2
import           Linear.Vector
import           System.Random

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
mainNewton = GG.simulate windowDisplay GG.white simulationRate initialModel drawingFunc updateFunc
  where
    initialModel :: Model
    initialModel = [Particle 1 (V2 0.0 0.0) (V2 1.0 0.0)]

    drawingFunc :: Model -> GG.Picture
    drawingFunc = GG.pictures . fmap drawParticle

    updateFunc :: ViewPort -> TimeStep -> Model -> Model
    updateFunc _ dt = newton dt

--

-- *** Helper functions

-- |

-- |
-- Update velocity of one `Particle`
-- assuming no acceleration
newton :: TimeStep -> [Particle] -> [Particle]
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
mainNewtonBounce = GG.simulate windowDisplay GG.white simulationRate initialModel drawingFunc updateFunc
  where
    initialModel :: Model
    initialModel = [Particle 1 (V2 0.0 0.0) (V2 1.0 0.0)]

    drawingFunc :: Model -> GG.Picture
    drawingFunc = GG.pictures . (:) drawWalls . fmap drawParticle

    updateFunc :: ViewPort -> Float -> Model -> Model
    updateFunc _ dt = newtonBounce dt

-- |

-- *** Additional helper functions

-- |

-- |
-- Updated `newton` function, which incorporates bouncing of the wall (see
-- `boundaryCondition`)
newtonBounce :: Float -> [Particle] -> [Particle]
newtonBounce dt [particle@(Particle idx pos vel)] = [Particle idx pos' vel']
  where
    transVec = boundaryCondition particle
    vel' = transVec * vel
    pos' = pos + vel' ^* dt
