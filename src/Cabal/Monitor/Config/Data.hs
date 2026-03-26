module Cabal.Monitor.Config.Data
  ( -- * Default
    Default (..),
    toMaybe,
    (<|.|>),

    -- * Data
    Coloring (..),
    Height (..),
    LocalPackages (..),
    Period (..),
    Pid (..),
    SearchInfix (..),
    Width (..),
  )
where

import Control.Applicative ((<|>))
import Numeric.Natural (Natural)

class Default a where
  def :: a

newtype SwitchOff = MkSwitchOff Bool

instance Default SwitchOff where
  def = MkSwitchOff False

newtype SwitchOn = MkSwitchOn Bool

instance Default SwitchOn where
  def = MkSwitchOn True

(<|.|>) :: (Default a) => Maybe a -> Maybe a -> a
l <|.|> r = toMaybe $ l <|> r

infixr 6 <|.|>

toMaybe :: (Default a) => Maybe a -> a
toMaybe Nothing = def
toMaybe (Just x) = x

newtype Coloring = MkColoring {unColoring :: Bool}
  deriving stock (Eq, Show)
  deriving (Default) via SwitchOn

newtype Height = MkHeight {unHeight :: Natural}
  deriving stock (Eq, Show)
  deriving newtype (Num)

newtype LocalPackages = MkLocalPackages {unLocalPackages :: Bool}
  deriving stock (Eq, Show)
  deriving (Default) via SwitchOn

newtype Period = MkPeriod {unPeriod :: Natural}
  deriving stock (Eq, Show)
  deriving newtype (Num)

instance Default Period where
  def = MkPeriod 5

newtype Pid = MkPid {unPid :: Natural}
  deriving stock (Eq, Show)
  deriving newtype (Num)

newtype SearchInfix = MkSearchInfix {unSearchInfix :: Bool}
  deriving stock (Eq, Show)
  deriving (Default) via SwitchOff

newtype Width = MkWidth {unWidth :: Natural}
  deriving stock (Eq, Show)
  deriving newtype (Num)
