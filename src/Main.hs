module Main where

import           Helpers ( mainNewton
                         , mainNewtonBounce
                         , mainVerlet
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
