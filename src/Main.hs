module Main where

import           Helpers ( mainVerlet
                         , mainVerletSquare
                         , mainVerletRandom
                         )

import           Newton ( mainNewton
                        , mainNewtonBounce
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
main = mainNewtonBounce
