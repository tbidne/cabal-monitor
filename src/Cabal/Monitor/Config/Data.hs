module Cabal.Monitor.Config.Data
  ( -- * Default
    Default (..),
    toMaybe,
    (<|.|>),

    -- * Data
    Coloring (..),
    Debug (..),
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

-- | Class for default values.
class Default a where
  def :: a

-- | Default 'off' switch, for deriving.
newtype SwitchOff = MkSwitchOff Bool

instance Default SwitchOff where
  def = MkSwitchOff False

-- | Default 'on' switch, for deriving.
newtype SwitchOn = MkSwitchOn Bool

instance Default SwitchOn where
  def = MkSwitchOn True

-- | Combines 'Maybe's via 'Alt', returning the result or 'Default' value.
(<|.|>) :: (Default a) => Maybe a -> Maybe a -> a
l <|.|> r = toMaybe $ l <|> r

infixr 6 <|.|>

-- | Returns the given value if it is 'Just', or 'def' if 'Nothing'.
toMaybe :: (Default a) => Maybe a -> a
toMaybe Nothing = def
toMaybe (Just x) = x

-- | Whether to color the logs.
newtype Coloring = MkColoring {unColoring :: Bool}
  deriving stock (Eq, Show)
  deriving (Default) via SwitchOn

newtype Debug = MkDebug {unDebug :: Bool}
  deriving stock (Eq, Show)
  deriving (Default) via SwitchOff

-- | Terminal height.
newtype Height = MkHeight {unHeight :: Natural}
  deriving stock (Eq, Show)
  deriving newtype (Num)

-- | Whether to search local packages.
newtype LocalPackages = MkLocalPackages {unLocalPackages :: Bool}
  deriving stock (Eq, Show)
  deriving (Default) via SwitchOn

-- | How often to read the status, in seconds.
newtype Period = MkPeriod {unPeriod :: Natural}
  deriving stock (Eq, Show)
  deriving newtype (Num)

instance Default Period where
  def = MkPeriod 5

-- | Pid of the process we are monitoring, for exiting automatically.
newtype Pid = MkPid {unPid :: Natural}
  deriving stock (Eq, Show)
  deriving newtype (Num)

-- | Whether to search logs for infix patterns, for more flexibility at
-- the cost of performance.
newtype SearchInfix = MkSearchInfix {unSearchInfix :: Bool}
  deriving stock (Eq, Show)
  deriving (Default) via SwitchOff

-- | Terminal width.
newtype Width = MkWidth {unWidth :: Natural}
  deriving stock (Eq, Show)
  deriving newtype (Num)
