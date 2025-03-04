module Monitor
  ( -- * High level
    runMonitor,

    -- * Low level

    -- ** Type
    Status (..),
    Package (..),
    FormatStyle (..),

    -- ** Functions
    monitorBuild,
    readFormattedStatus,
    parseStatus,
    formatStatus,
  )
where

import Control.DeepSeq (NFData)
import Control.Monad (forever)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Char8 qualified as C8
import Data.ByteString.Lazy qualified as BSL
import Data.Foldable qualified as F
import Data.Maybe (fromMaybe)
import Data.Sequence (Seq (Empty, (:<|), (:|>)))
import Data.Sequence qualified as Seq
import Data.Set (Set, (\\))
import Data.Set qualified as Set
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Relative qualified as Rel
import Effectful (Eff, type (:>))
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent qualified as CC
import Effectful.Concurrent.Async qualified as Async
import Effectful.Dispatch.Dynamic (HasCallStack)
import Effectful.Exception qualified as Ex
import Effectful.FileSystem.FileReader.Static (FileReader)
import Effectful.FileSystem.FileReader.Static qualified as FR
import Effectful.Optparse.Static (Optparse)
import Effectful.State.Static.Local qualified as State
import Effectful.Terminal.Dynamic (Terminal)
import Effectful.Terminal.Dynamic qualified as Term
import FileSystem.OsPath (OsPath)
import FileSystem.UTF8 qualified as UTF8
import GHC.Generics (Generic)
import GHC.Natural (Natural)
import Monitor.Args (Args (filePath, height, period, width))
import Monitor.Args qualified as Args
import Monitor.Logger (LogMode (LogModeSet), RegionLogger)
import Monitor.Logger qualified as Logger
import System.Console.Regions (RegionLayout (Linear))

runMonitor ::
  forall r ->
  forall es void.
  ( Concurrent :> es,
    FileReader :> es,
    HasCallStack,
    Optparse :> es,
    RegionLogger r :> es,
    Terminal :> es
  ) =>
  Eff es void
runMonitor rType = do
  args <- Args.getArgs
  monitorBuild rType args

monitorBuild ::
  forall r ->
  forall es void.
  ( Concurrent :> es,
    FileReader :> es,
    HasCallStack,
    RegionLogger r :> es,
    Terminal :> es
  ) =>
  Args ->
  Eff es void
monitorBuild rType args =
  Logger.displayRegions rType $ do
    eResult <-
      Async.race
        runStatus
        (logCounter rType)

    case eResult of
      Left e -> pure e
      Right x -> pure x
  where
    runStatus = Logger.withRegion @rType Linear $ \r -> forever $ do
      readPrintStatus r args.height args.width args.filePath
      CC.threadDelay period_ms

    period_ms = 1_000_000 * (fromMaybe 5 args.period)

readPrintStatus ::
  ( FileReader :> es,
    HasCallStack,
    RegionLogger r :> es,
    Terminal :> es
  ) =>
  r ->
  Maybe Int ->
  Maybe Int ->
  OsPath ->
  Eff es ()
readPrintStatus region mHeight mWidth path = do
  formatted <- readFormattedStatus mHeight mWidth path
  Logger.logRegion LogModeSet region formatted

readFormattedStatus ::
  ( FileReader :> es,
    HasCallStack,
    Terminal :> es
  ) =>
  Maybe Int ->
  Maybe Int ->
  OsPath ->
  Eff es Text
readFormattedStatus mHeight mWidth path = do
  status <- readStatus path

  style <- case (mHeight, mWidth) of
    (Just height, Just width) -> pure $ FormatInlTrunc height width
    (Just height, Nothing) -> pure $ FormatNlTrunc height
    (Nothing, Just width) -> pure $ FormatInl width
    (Nothing, Nothing) -> do
      eResult <- Ex.trySync $ Term.getTerminalSize
      pure $ case eResult of
        Left _ -> FormatNl
        Right sz -> do
          -- - 4 (To Build, Building, Completed, Timer) stanzas with a header
          --   and trailing newline --> 4 * 2 == 8
          --
          --   --> 4 * 2 == 8
          let availPkgLines = sz.height `monus` 8
              neededLines = length status.allLibs

          if neededLines < availPkgLines
            -- 2.2. Normal, non-compact format fits in the vertical space;
            --      use it.
            then FormatNl
            -- 2.3. Does not fit in vertical space. Use compact, with length
            --      determined by terminal size.
            else FormatInlTrunc (sz.height - 1) (sz.width - 1)

  pure $ formatStatus style status

newtype Package = MkPackage {unPackage :: ByteString}
  deriving stock (Eq, Generic, Ord, Show)
  deriving newtype (IsString)
  deriving anyclass (NFData)

data Status = MkStatus
  { allLibs :: Set Package,
    building :: Set Package,
    completed :: Set Package
  }
  deriving stock (Eq, Generic, Show)
  deriving anyclass (NFData)

instance Semigroup Status where
  MkStatus x1 x2 x3 <> MkStatus y1 y2 y3 =
    MkStatus (x1 <> y1) (x2 <> y2) (x3 <> y3)

instance Monoid Status where
  mempty = MkStatus mempty mempty mempty

readStatus ::
  ( FileReader :> es,
    HasCallStack
  ) =>
  OsPath ->
  Eff es Status
readStatus path = do
  contents <- FR.readBinaryFile path
  pure $ parseStatus $ C8.lines contents

logCounter ::
  forall r ->
  forall es void.
  ( Concurrent :> es,
    HasCallStack,
    RegionLogger r :> es
  ) =>
  Eff es void
logCounter rType = do
  CC.threadDelay 100_000
  Logger.withRegion @rType Linear $ \r ->
    State.evalState @Natural 0 $
      forever $
        do
          CC.threadDelay 1_000_000
          -- IDEA: Could use shared state and detect a file change, in which
          -- case we reset the timer?
          State.modify @Natural (\(!x) -> x + 1)
          elapsed <- State.get
          Logger.logRegion
            LogModeSet
            r
            (fmtTime elapsed)
  where
    fmtTime s =
      (\t -> "\nTimer: " <> t)
        . T.pack
        . Rel.formatSeconds Rel.defaultFormat
        $ s

parseStatus :: [ByteString] -> Status
parseStatus = F.foldMap' go
  where
    go txt = case BS.stripPrefix " - " txt of
      Just rest -> mkLib (BS.takeWhile (/= 32) rest)
      Nothing -> case BS.stripPrefix "Building" txt of
        Just rest -> mkBuilding (takeSkipLeadingSpc rest)
        Nothing -> case BS.stripPrefix "Completed" txt of
          Just rest -> mkCompleted (takeSkipLeadingSpc rest)
          Nothing -> mempty

    takeSkipLeadingSpc = BS.takeWhile (/= 32) . BS.dropWhile (== 32)

data FormatStyle
  = -- | Format each package on a newline.
    FormatNl
  | -- | Format each package on a newline, truncating lines exceeding the
    -- given height.
    FormatNlTrunc Int
  | -- | Format packages inline, creating a newline once we exceed the
    -- given width.
    FormatInl Int
  | -- | Given height and width, formats inline (width) and truncates
    -- (height).
    FormatInlTrunc Int Int

formatStatus :: FormatStyle -> Status -> Text
formatStatus style status =
  UTF8.unsafeDecodeUtf8 $
    BSL.toStrict $
      BSB.toLazyByteString $
        formatAll
          style
          toBuild
          building
          completed
  where
    toBuild =
      status.allLibs
        \\ (status.building `Set.union` status.completed)
    building = status.building \\ status.completed
    completed = status.completed

formatAll ::
  FormatStyle ->
  Set Package ->
  Set Package ->
  Set Package ->
  Builder
formatAll style toBuildS buildingS completedS = final
  where
    final =
      concatSeq $
        concatter
          toBuildBuilders
          buildingBuilders
          completedBuilders

    (formatter, concatter) = case style of
      FormatNl -> (formatNewlines, concatNewlines)
      FormatNlTrunc height -> (formatNewlines, concatCompact height)
      FormatInl width -> (formatCompact width, concatNewlines)
      FormatInlTrunc height width -> (formatCompact width, concatCompact height)

    concatSeq :: Seq Builder -> Builder
    concatSeq = F.foldMap' id

    concatNewlines :: Seq Builder -> Seq Builder -> Seq Builder -> Seq Builder
    concatNewlines as bs cs =
      as <> ("\n" :<| "\n" :<| Empty) <> bs <> ("\n" :<| "\n" :<| Empty) <> cs

    concatCompact :: Int -> Seq Builder -> Seq Builder -> Seq Builder -> Seq Builder
    concatCompact height as bs cs =
      let (h1, bs') = takeCount height bs
          hEach = h1 `div` 2
          as' = takeTrunc hEach as
          cs' = takeTrunc hEach cs
       in concatNewlines as' bs' cs'

    toBuildL = Seq.fromList $ Set.toList toBuildS
    toBuildBuilders =
      let bs = formatter toBuildL
       in "To Build: " <> (BSB.intDec $ Set.size toBuildS) :<| bs

    buildingL = Seq.fromList $ Set.toList buildingS
    buildingBuilders =
      let bs = formatter buildingL
       in "Building: " <> (BSB.intDec $ Set.size buildingS) :<| bs

    completedL = Seq.fromList $ Set.toList completedS
    completedBuilders =
      let bs = formatter completedL
       in "Completed: " <> (BSB.intDec $ Set.size completedS) :<| bs

formatNewlines :: Seq Package -> Seq Builder
formatNewlines = foldl' go Empty
  where
    go acc p = acc :|> prependNewline (BSB.byteString p.unPackage)

formatCompact :: Int -> Seq Package -> Seq Builder
formatCompact width = fmap fst . foldl' go Empty
  where
    go :: Seq (Builder, Int) -> Package -> Seq (Builder, Int)
    go Empty p =
      let (newBuilder, newLen) = pkgToData p
       in Seq.singleton (prependNewline newBuilder, newLen + newlineIdent)
    go (accs :|> (currBuilder, currLen)) p =
      let (newBuilder, newLen) = pkgToData p
          totalLen = newLen + currLen + newPkgIdent
       in if totalLen + 1 > width
            then
              accs
                :|> (currBuilder, currLen)
                :|> (prependNewline newBuilder, newLen + newlineIdent)
            else
              accs :|> (currBuilder <> ", " <> newBuilder, totalLen)

    newlineIdent = 4
    newPkgIdent = 2

    pkgToData p =
      let bs = p.unPackage
       in (BSB.byteString bs, BS.length bs)

prependNewline :: Builder -> Builder
prependNewline b = "\n  - " <> b

mkLib :: ByteString -> Status
mkLib lib =
  MkStatus
    { allLibs = Set.singleton $ MkPackage lib,
      building = mempty,
      completed = mempty
    }

mkBuilding :: ByteString -> Status
mkBuilding lib =
  MkStatus
    { allLibs = mempty,
      building = Set.singleton $ MkPackage lib,
      completed = mempty
    }

mkCompleted :: ByteString -> Status
mkCompleted lib =
  MkStatus
    { allLibs = mempty,
      building = mempty,
      completed = Set.singleton $ MkPackage lib
    }

takeCount :: Int -> Seq a -> (Int, Seq a)
takeCount k = go (k, Empty)
  where
    go acc Empty = acc
    go acc@(0, _) _ = acc
    go (!cnt, zs) (x :<| xs) = go (cnt - 1, (zs :|> x)) xs

takeTrunc :: Int -> Seq Builder -> Seq Builder
takeTrunc 0 acc = acc
-- 2. No more to take.
takeTrunc _ Empty = Empty
-- 3. Can take 1 but more than one left: add ellipsis.
takeTrunc 1 (_ :<| _ :<| _) = Seq.singleton $ "\n  ..."
-- 4. General case: take 1, recurse.
takeTrunc !n (b :<| bs) = b :<| takeTrunc (n - 1) bs

-- FIXME: Ints should be Word or something

monus :: (Num a, Ord a) => a -> a -> a
monus x y
  | x < y = 0
  | otherwise = x - y
