module Main where

import Prelude

import Effect.Console (logShow)
import Math (sqrt, pi)

diagonal :: Number -> Number -> Number
diagonal w h = sqrt (w * w + h * h)

circle r = pi * r * r

main = logShow (diagonal 3.0 4.0)
