{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Cabal.Monitor qualified as Monitor
import Cabal.Monitor.BuildStatus
  ( BuildStatus (MkBuildStatus, building, completed, toBuild),
    BuildStatusInit,
    FormatStyle
      ( FormatInl,
        FormatInlTrunc,
        FormatNl,
        FormatNlTrunc
      ),
  )
import Cabal.Monitor.BuildStatus qualified as BuildStatus
import Cabal.Monitor.Logger
  ( RegionLogger
      ( DisplayRegions,
        LogRegion,
        WithRegion
      ),
  )
import Cabal.Monitor.Pretty qualified as Pretty
import Control.Monad (unless, void)
import Data.Foldable (for_)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as T
import Effectful (Eff, IOE, MonadIO (liftIO), runEff, type (:>))
import Effectful.Concurrent qualified as ECC
import Effectful.Concurrent.Async qualified as EAsync
import Effectful.Dispatch.Dynamic (interpret, interpret_, localSeqUnlift)
import Effectful.Dynamic.Utils (ShowEffect (showEffectCons))
import Effectful.FileSystem.FileReader.Static qualified as FR
import Effectful.FileSystem.PathReader.Static (PathReader)
import Effectful.FileSystem.PathReader.Static qualified as PR
import Effectful.FileSystem.PathWriter.Static qualified as PW
import Effectful.Optparse.Static qualified as EOA
import Effectful.Process (Process)
import Effectful.Process qualified as EProcess
import Effectful.Terminal.Dynamic
  ( Terminal (GetTerminalSize),
    Window (Window, height, width),
  )
import FileSystem.OsPath
  ( OsPath,
    decodeLenient,
    decodeThrowM,
    ospPathSep,
    (</>),
  )
import System.Environment qualified as Env
import System.Environment.Guard (ExpectEnv (ExpectEnvSet), guardOrElse')
import System.Timeout qualified as TO
import Test.Tasty
  ( TestTree,
    defaultMain,
    testGroup,
    withResource,
  )
import Test.Tasty.HUnit (HasCallStack, assertFailure, testCase, (@=?))

main :: IO ()
main =
  defaultMain (withResource setup teardown tests)
  where
    tests getTestArgs =
      testGroup
        "Functional"
        [ monitorTests getTestArgs,
          formatStatusTests
        ]

monitorTests :: IO TestArgs -> TestTree
monitorTests getTestArgs =
  testGroup
    "monitor"
    [ testMonitor getTestArgs,
      testMonitorShortWindow getTestArgs
    ]

testMonitor :: IO TestArgs -> TestTree
testMonitor getTestArgs = testCase "Monitors build output" $ do
  testArgs <- getTestArgs
  let buildOsPath = testArgs.tmpDir </> [ospPathSep|testMonitor.txt|]
  buildFilePath <- decodeThrowM buildOsPath

  logs <- runMonitorLogs getTestArgs buildOsPath (args buildFilePath)

  for_ expected $ \e -> do
    unless (containsLog e logs) $ do
      let msg =
            T.unpack $
              mconcat
                [ "*** Did not find log: ***\n\n",
                  e,
                  "\n\n*** In logs: ***\n\n",
                  T.intercalate "\n\n" (Set.toList logs)
                ]
      assertFailure msg
  where
    args p =
      [ "--file",
        p,
        "--period",
        "1"
      ]

    expected = [e0, e1, e2, e3, e4, e5, t0, t1]

    t0 = "Waiting to start:"

    t1 = "Building: 1 second"

    -- NOTE: No "Finished in" log because its presence is non-deterministic
    -- (based on timing).

    e0 = "Build file does not exist at path:"

    e1 =
      unlineStrip
        [ Pretty.magenta <> "To Build: 5",
          "  - bits-0.6",
          "  - byteable-0.1.1",
          "  - indexed-profunctors-0.1.1.1",
          "  - mtl-compat-0.2.2",
          "  - string-qq-0.0.6" <> Pretty.endCode,
          "",
          Pretty.yellow <> "Building: 0" <> Pretty.endCode,
          "",
          Pretty.green <> "Completed: 0" <> Pretty.endCode
        ]
    e2 =
      unlineStrip
        [ Pretty.magenta <> "To Build: 2",
          "  - mtl-compat-0.2.2",
          "  - string-qq-0.0.6" <> Pretty.endCode,
          "",
          Pretty.yellow <> "Building: 3",
          "  - bits-0.6",
          "  - byteable-0.1.1",
          "  - indexed-profunctors-0.1.1.1" <> Pretty.endCode,
          "",
          Pretty.green <> "Completed: 0" <> Pretty.endCode
        ]

    e3 =
      unlineStrip
        [ Pretty.magenta <> "To Build: 1",
          "  - string-qq-0.0.6" <> Pretty.endCode,
          "",
          Pretty.yellow <> "Building: 4",
          "  - bits-0.6",
          "  - byteable-0.1.1",
          "  - indexed-profunctors-0.1.1.1",
          "  - mtl-compat-0.2.2" <> Pretty.endCode,
          "",
          Pretty.green <> "Completed: 0" <> Pretty.endCode
        ]

    e4 =
      unlineStrip
        [ Pretty.magenta <> "To Build: 1",
          "  - string-qq-0.0.6" <> Pretty.endCode,
          "",
          Pretty.yellow <> "Building: 2",
          "  - indexed-profunctors-0.1.1.1",
          "  - mtl-compat-0.2.2" <> Pretty.endCode,
          "",
          Pretty.green <> "Completed: 2",
          "  - bits-0.6",
          "  - byteable-0.1.1" <> Pretty.endCode
        ]

    e5 =
      unlineStrip
        [ Pretty.magenta <> "To Build: 0" <> Pretty.endCode,
          "",
          Pretty.yellow <> "Building: 2",
          "  - mtl-compat-0.2.2",
          "  - string-qq-0.0.6" <> Pretty.endCode,
          "",
          Pretty.green <> "Completed: 3",
          "  - bits-0.6",
          "  - byteable-0.1.1",
          "  - indexed-profunctors-0.1.1.1" <> Pretty.endCode
        ]

testMonitorShortWindow :: IO TestArgs -> TestTree
testMonitorShortWindow getTestArgs = testCase "Monitors with short window" $ do
  testArgs <- getTestArgs
  let buildOsPath = testArgs.tmpDir </> [ospPathSep|testMonitorShortWindow.txt|]
  buildFilePath <- decodeThrowM buildOsPath

  let testArgs' =
        testArgs
          { mWindow =
              Just
                ( Window
                    { height = 10,
                      width = 80
                    }
                )
          }

  logs <- runMonitorLogs (pure testArgs') buildOsPath (args buildFilePath)

  for_ expected $ \e -> do
    unless (containsLog e logs) $ do
      let msg =
            T.unpack $
              mconcat
                [ "*** Did not find log: ***\n\n",
                  e,
                  "\n\n*** In logs: ***\n\n",
                  T.intercalate "\n\n" (Set.toList logs)
                ]
      assertFailure msg
  where
    args p =
      [ "--file",
        p,
        "--period",
        "1"
      ]

    expected = [e0, e1, e2, e3, e4, e5, t0, t1]

    t0 = "Waiting to start:"

    t1 = "Building: 1 second"

    e0 = "Build file does not exist at path:"

    e1 =
      unlineStrip
        [ Pretty.magenta <> "To Build: 5",
          "  - bits-0.6, byteable-0.1.1, indexed-profunctors-0.1.1.1, mtl-compat-0.2.2",
          "  - string-qq-0.0.6" <> Pretty.endCode,
          "",
          Pretty.yellow <> "Building: 0" <> Pretty.endCode,
          "",
          Pretty.green <> "Completed: 0" <> Pretty.endCode
        ]
    e2 =
      unlineStrip
        [ Pretty.magenta <> "To Build: 2",
          "  - mtl-compat-0.2.2, string-qq-0.0.6" <> Pretty.endCode,
          "",
          Pretty.yellow <> "Building: 3",
          "  - bits-0.6, byteable-0.1.1, indexed-profunctors-0.1.1.1" <> Pretty.endCode,
          "",
          Pretty.green <> "Completed: 0" <> Pretty.endCode
        ]

    e3 =
      unlineStrip
        [ Pretty.magenta <> "To Build: 1",
          "  - string-qq-0.0.6" <> Pretty.endCode,
          "",
          Pretty.yellow <> "Building: 4",
          "  - bits-0.6, byteable-0.1.1, indexed-profunctors-0.1.1.1, mtl-compat-0.2.2" <> Pretty.endCode,
          "",
          Pretty.green <> "Completed: 0" <> Pretty.endCode
        ]

    e4 =
      unlineStrip
        [ Pretty.magenta <> "To Build: 1",
          "  - string-qq-0.0.6" <> Pretty.endCode,
          "",
          Pretty.yellow <> "Building: 2",
          "  - indexed-profunctors-0.1.1.1, mtl-compat-0.2.2" <> Pretty.endCode,
          "",
          Pretty.green <> "Completed: 2",
          "  - bits-0.6, byteable-0.1.1" <> Pretty.endCode
        ]

    e5 =
      unlineStrip
        [ Pretty.magenta <> "To Build: 0" <> Pretty.endCode,
          "",
          Pretty.yellow <> "Building: 2",
          "  - mtl-compat-0.2.2, string-qq-0.0.6" <> Pretty.endCode,
          "",
          Pretty.green <> "Completed: 3",
          "  - bits-0.6, byteable-0.1.1, indexed-profunctors-0.1.1.1" <> Pretty.endCode
        ]

formatStatusTests :: TestTree
formatStatusTests =
  testGroup
    "formatStatus"
    [ testFormatNl,
      testFormatNlTrunc,
      testFormatInl,
      testFormatInlTrunc
    ]

testFormatNl :: TestTree
testFormatNl = testCase desc $ do
  expected @=? BuildStatus.formatStatusInit FormatNl exampleStatus
  where
    desc = "Formats with newlines"
    expected =
      unlineStrip
        [ Pretty.magenta <> "To Build: 8",
          "  - lib13",
          "  - lib14",
          "  - lib15",
          "  - lib16",
          "  - lib17",
          "  - lib18",
          "  - lib19",
          "  - lib20" <> Pretty.endCode,
          "",
          Pretty.yellow <> "Building: 7",
          "  - lib10",
          "  - lib11",
          "  - lib12",
          "  - lib6",
          "  - lib7",
          "  - lib8",
          "  - lib9" <> Pretty.endCode,
          "",
          Pretty.green <> "Completed: 5",
          "  - lib1",
          "  - lib2",
          "  - lib3",
          "  - lib4",
          "  - lib5" <> Pretty.endCode
        ]

testFormatNlTrunc :: TestTree
testFormatNlTrunc = testCase desc $ do
  expected @=? BuildStatus.formatStatusInit (FormatNlTrunc 15) exampleStatus
  where
    desc = "Formats with newlines and truncation"
    expected =
      unlineStrip
        [ Pretty.magenta <> "To Build: 8",
          "  - lib13",
          "  ..." <> Pretty.endCode,
          "",
          Pretty.yellow <> "Building: 7",
          "  - lib10",
          "  - lib11",
          "  - lib12",
          "  - lib6",
          "  - lib7",
          "  - lib8",
          "  - lib9" <> Pretty.endCode,
          "",
          Pretty.green <> "Completed: 5",
          "  - lib1",
          "  ..." <> Pretty.endCode
        ]

testFormatInl :: TestTree
testFormatInl = testCase desc $ do
  expected @=? BuildStatus.formatStatusInit (FormatInl 25) exampleStatus
  where
    desc = "Formats with inline"
    expected =
      unlineStrip
        [ Pretty.magenta <> "To Build: 8",
          "  - lib13, lib14, lib15",
          "  - lib16, lib17, lib18",
          "  - lib19, lib20" <> Pretty.endCode,
          "",
          Pretty.yellow <> "Building: 7",
          "  - lib10, lib11, lib12",
          "  - lib6, lib7, lib8",
          "  - lib9" <> Pretty.endCode,
          "",
          Pretty.green <> "Completed: 5",
          "  - lib1, lib2, lib3",
          "  - lib4, lib5" <> Pretty.endCode
        ]

testFormatInlTrunc :: TestTree
testFormatInlTrunc = testCase desc $ do
  expected @=? BuildStatus.formatStatusInit (FormatInlTrunc 11 25) exampleStatus
  where
    desc = "Formats with inline and truncation"
    expected =
      unlineStrip
        [ Pretty.magenta <> "To Build: 8",
          "  - lib13, lib14, lib15",
          "  ..." <> Pretty.endCode,
          "",
          Pretty.yellow <> "Building: 7",
          "  - lib10, lib11, lib12",
          "  - lib6, lib7, lib8",
          "  - lib9" <> Pretty.endCode,
          "",
          Pretty.green <> "Completed: 5",
          "  - lib1, lib2, lib3",
          "  - lib4, lib5" <> Pretty.endCode
        ]

exampleStatus :: BuildStatusInit
exampleStatus =
  MkBuildStatus
    { toBuild = Set.fromList allPkgsL,
      building = Set.fromList (take 12 allPkgsL),
      completed = Set.fromList (take 5 allPkgsL)
    }
  where
    allPkgsL = (fromString . ("lib" <>) . show @Int) <$> [1 .. 20]

type Unit = ()

runMonitorLogs ::
  (HasCallStack) =>
  IO TestArgs -> OsPath -> [String] -> IO (Set Text)
runMonitorLogs getTestArgs buildFileOsPath cliArgs = do
  testArgs <- getTestArgs
  logsRef <- newIORef mempty
  _ <-
    TO.timeout 13_000_000 $
      ( Env.withArgs cliArgs $ runner logsRef testArgs.mWindow $ do
          (runBuildScript buildFileOsPath)
            `EAsync.concurrently`
            -- start this second so build file exists.
            (ECC.threadDelay 500_000 *> Monitor.runMonitor Unit)
      )
  readIORef logsRef
  where
    runner ref mWindow =
      runEff
        . ECC.runConcurrent
        . EProcess.runProcess
        . runTerminalMock mWindow
        . runRegionLoggerMock ref
        . PR.runPathReader
        . FR.runFileReader
        . EOA.runOptparse

runRegionLoggerMock ::
  forall r es a.
  ( r ~ (),
    HasCallStack,
    IOE :> es
  ) =>
  IORef (Set Text) ->
  Eff (RegionLogger r : es) a ->
  Eff es a
runRegionLoggerMock logsRef = interpret $ \env -> \case
  LogRegion _ _ t -> writeLogs t
  WithRegion _ onRegion -> localSeqUnlift env $ \unlift ->
    unlift (onRegion ())
  DisplayRegions m -> localSeqUnlift env $ \unlift -> unlift m
  where
    writeLogs :: Text -> Eff es ()
    writeLogs txt = liftIO $ do
      modifyIORef' logsRef (\st -> Set.insert txt st)

runTerminalMock :: (Integral b) => Maybe (Window b) -> Eff (Terminal : es) a -> Eff es a
runTerminalMock mWindow = interpret_ $ \case
  GetTerminalSize ->
    pure $ case mWindow of
      Just window ->
        Window
          { height = fromIntegral window.height,
            width = fromIntegral window.width
          }
      Nothing -> Window {height = 80, width = 100}
  other -> error $ showEffectCons other

runBuildScript ::
  ( HasCallStack,
    PathReader :> es,
    Process :> es
  ) =>
  OsPath ->
  Eff es ()
runBuildScript buildFileOsPath = do
  pwd <- PR.getCurrentDirectory
  scriptPath <- decodeThrowM (pwd </> [ospPathSep|test/functional/build.sh|])

  outPath <- decodeThrowM buildFileOsPath

  void $ EProcess.createProcess $ EProcess.proc scriptPath [outPath]

data TestArgs = MkTestArgs
  { tmpDir :: OsPath,
    mWindow :: Maybe (Window Int)
  }

setup :: (HasCallStack) => IO TestArgs
setup = runTestEff $ do
  tmpDir <- (</> [ospPathSep|cabal-monitor|]) <$> PR.getTemporaryDirectory

  PW.removePathForciblyIfExists_ tmpDir

  PW.createDirectoryIfMissing True tmpDir

  pure $ MkTestArgs tmpDir Nothing

teardown :: (HasCallStack) => TestArgs -> IO ()
teardown testArgs = do
  let cleanup = runTestEff . PW.removePathForciblyIfExists_ $ testArgs.tmpDir
      doNothing =
        putStrLn $
          "*** Not cleaning up tmp dir: '"
            <> decodeLenient testArgs.tmpDir
            <> "'"

  guardOrElse' "NO_CLEANUP" ExpectEnvSet doNothing cleanup

runTestEff ::
  (HasCallStack) =>
  Eff [PR.PathReader, PW.PathWriter, IOE] a -> IO a
runTestEff =
  runEff
    . PW.runPathWriter
    . PR.runPathReader

unlineStrip :: [Text] -> Text
unlineStrip = T.strip . T.unlines

containsLog :: Text -> Set Text -> Bool
containsLog l logs =
  if l `Set.member` logs
    then True
    -- O(n) fallback for inexact match.
    else any (T.isInfixOf l) logs
