module Monitor
  ( monitorBuild,
  )
where

import Control.Concurrent qualified as CC
import Control.Monad (forever)
import Data.Foldable (foldMap')
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as TLB
import FileSystem.IO qualified as IO
import FileSystem.OsPath (OsPath)
import FileSystem.UTF8 qualified as UTF8
import System.Console.Regions (ConsoleRegion, RegionLayout (Linear))
import System.Console.Regions qualified as Regions

monitorBuild :: OsPath -> IO void
monitorBuild path =
  Regions.displayConsoleRegions $
    Regions.withConsoleRegion Linear $ \r -> forever $ do
      readPrintStatus r path
      CC.threadDelay 5_000_000

readPrintStatus :: ConsoleRegion -> OsPath -> IO ()
readPrintStatus region path = do
  status <- readStatus path
  let formatted = fmtStatus status
  Regions.setConsoleRegion region formatted

newtype Package = MkPackage {unPackage :: Text}
  deriving stock (Eq, Ord, Show)

data Status = MkStatus
  { allLibs :: Set Package,
    building :: Set Package,
    completed :: Set Package
  }
  deriving stock (Eq, Show)

instance Semigroup Status where
  MkStatus x1 x2 x3 <> MkStatus y1 y2 y3 =
    MkStatus (x1 <> y1) (x2 <> y2) (x3 <> y3)

instance Monoid Status where
  mempty = MkStatus mempty mempty mempty

readStatus :: OsPath -> IO Status
readStatus path = do
  contents <- UTF8.decodeUtf8ThrowM =<< IO.readBinaryFileIO path
  pure $ parseStatus $ T.lines contents

parseStatus :: [Text] -> Status
parseStatus = foldMap' go
  where
    go txt = case T.stripPrefix " - " txt of
      Just rest -> mkLib (T.takeWhile (/= ' ') rest)
      Nothing -> case T.stripPrefix "Building" txt of
        Just rest -> mkBuilding (takeSkipLeadingSpc rest)
        Nothing -> case T.stripPrefix "Completed" txt of
          Just rest -> mkCompleted (takeSkipLeadingSpc rest)
          Nothing -> mempty

    takeSkipLeadingSpc = T.takeWhile (/= ' ') . T.dropWhile (== ' ')

fmtStatus :: Status -> Text
fmtStatus status =
  TL.toStrict $
    TLB.toLazyText $
      mconcat
        [ "Packages: " <> showtlb (length status.allLibs),
          "\nCompleted: " <> showtlb (length status.completed),
          "\nBuilding " <> numBuilding <> ": " <> fmtBuilding (Set.toList building),
          "\n"
        ]
  where
    numBuilding = showtlb (length building)
    building =
      Set.filter
        (\p -> not (p `Set.member` status.completed))
        status.building

    fmtBuilding :: [Package] -> Builder
    fmtBuilding [] = ""
    fmtBuilding xs =
      mconcat $
        fmap (\p -> "\n - " <> TLB.fromText p.unPackage) xs

    showtlb :: forall a. (Show a) => a -> Builder
    showtlb = TLB.fromString . show

mkLib :: Text -> Status
mkLib lib =
  MkStatus
    { allLibs = Set.singleton $ MkPackage lib,
      building = mempty,
      completed = mempty
    }

mkBuilding :: Text -> Status
mkBuilding lib =
  MkStatus
    { allLibs = mempty,
      building = Set.singleton $ MkPackage lib,
      completed = mempty
    }

mkCompleted :: Text -> Status
mkCompleted lib =
  MkStatus
    { allLibs = mempty,
      building = mempty,
      completed = Set.singleton $ MkPackage lib
    }
