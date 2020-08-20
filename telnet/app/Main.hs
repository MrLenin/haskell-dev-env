module Main where

import           Lib
import           Prelude


multiPi :: Floating a => a -> a
multiPi a = pi * (a * a)

main :: IO ()
main = someFunc
