{-# LANGUAGE QuasiQuotes #-}

{- HLINT ignore "Functor law" -}

module Cabal.Monitor.Config.Toml
  ( Toml (..),
    getTomlConfig,
  )
where

import Cabal.Monitor.Config.Data
  ( Coloring (MkColoring),
    Height (MkHeight),
    LocalPackages (MkLocalPackages),
    Period (MkPeriod),
    SearchInfix (MkSearchInfix),
    Width (MkWidth),
  )
import Control.Category ((>>>))
import Data.Maybe (fromMaybe)
import Effectful (Eff, type (:>))
import Effectful.Dispatch.Dynamic (HasCallStack)
import Effectful.Exception qualified as Ex
import Effectful.FileSystem.FileReader.Static (FileReader)
import Effectful.FileSystem.FileReader.Static qualified as FR
import Effectful.FileSystem.PathReader.Dynamic (PathReader)
import Effectful.FileSystem.PathReader.Dynamic qualified as PR
import FileSystem.OsPath (OsPath, ospPathSep)
import TOML
  ( DecodeTOML (tomlDecoder),
    Decoder,
    decode,
    getFieldOptWith,
  )

data Toml = MkToml
  { coloring :: Maybe Coloring,
    height :: Maybe Height,
    localPackages :: Maybe LocalPackages,
    period :: Maybe Period,
    searchInfix :: Maybe SearchInfix,
    width :: Maybe Width
  }
  deriving stock (Eq, Show)

instance DecodeTOML Toml where
  tomlDecoder = do
    (localPackages, period, searchInfix) <- decodeCore
    (coloring, height, width) <- decodeFormatting
    pure $
      MkToml
        { coloring,
          height,
          localPackages,
          period,
          searchInfix,
          width
        }
    where
      decodeCore =
        fmap (fromMaybe (Nothing, Nothing, Nothing)) $
          flip getFieldOptWith "core" $ do
            (,,)
              <$> (fmap MkLocalPackages <$> getFieldOptWith decodeSwitch "local-packages")
              <*> (fmap MkPeriod <$> getFieldOptWith tomlDecoder "period")
              <*> (fmap MkSearchInfix <$> getFieldOptWith decodeSwitch "search-infix")

      decodeFormatting =
        fmap (fromMaybe (Nothing, Nothing, Nothing)) $
          flip getFieldOptWith "formatting" $ do
            (,,)
              <$> (fmap MkColoring <$> getFieldOptWith decodeSwitch "color")
              <*> (fmap MkHeight <$> getFieldOptWith tomlDecoder "height")
              <*> (fmap MkWidth <$> getFieldOptWith tomlDecoder "width")

decodeSwitch :: Decoder Bool
decodeSwitch =
  tomlDecoder >>= \case
    "off" -> pure False
    "on" -> pure True
    other -> fail $ "Expected (on | off), received: " <> other

getTomlConfig ::
  ( FileReader :> es,
    HasCallStack,
    PathReader :> es
  ) =>
  Maybe OsPath ->
  Eff es (Maybe Toml)
getTomlConfig (Just path) = Just <$> readTomlConfig path
getTomlConfig Nothing = do
  xdgConfigPath <- PR.getXdgConfig [ospPathSep|cabal-monitor/config.toml|]
  exists <- PR.doesFileExist xdgConfigPath
  if exists
    then Just <$> readTomlConfig xdgConfigPath
    else pure Nothing

readTomlConfig :: (FileReader :> es, HasCallStack) => OsPath -> Eff es Toml
readTomlConfig path = do
  FR.readFileUtf8ThrowM path
    >>= ( decode >>> \case
            Left ex -> Ex.throwIO ex
            Right x -> pure x
        )
