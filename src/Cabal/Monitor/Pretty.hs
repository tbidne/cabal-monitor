module Cabal.Monitor.Pretty
  ( -- * Colors
    Color (..),
    blue,
    green,
    magenta,
    yellow,

    -- * General
    color,
    colorToCode,
    endCode,
  )
where

import Data.String (IsString)

data Color
  = Blue
  | Green
  | Magenta
  | Yellow

colorToCode :: (IsString a) => Color -> a
colorToCode Blue = "\ESC[34m"
colorToCode Green = "\ESC[32m"
colorToCode Magenta = "\ESC[35m"
colorToCode Yellow = "\ESC[33m"

endCode :: (IsString a) => a
endCode = "\ESC[0m"

blue :: (IsString a) => a
blue = colorToCode Blue

green :: (IsString a) => a
green = colorToCode Green

magenta :: (IsString a) => a
magenta = colorToCode Magenta

yellow :: (IsString a) => a
yellow = colorToCode Yellow

color :: (IsString a, Semigroup a) => Color -> a -> a
color c bs = colorToCode c <> bs <> endCode
