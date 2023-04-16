module Main where

import           Newton ( mainNewton
                        , mainNewtonBounce
                        )

import           Verlet ( mainVerlet
                        , mainVerletSquare
                        , mainVerletRandom
                        )

-- |
-- Select the main function of your choice
--
--    * 'mainNewton'
--    * 'mainNewtonBounce'
--    * 'mainVerlet'
--    * 'mainVerletSquare'
--    * 'mainVerletRandom'
--
-- to perform the according simulation
main :: IO ()
main = mainVerletRandom
