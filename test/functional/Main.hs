{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Cabal.Monitor qualified as Monitor
import Cabal.Monitor.Args (Coloring (MkColoring))
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
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C8
import Data.Foldable qualified as F
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.List qualified as L
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
import Effectful.FileSystem.FileWriter.Static qualified as FW
import Effectful.FileSystem.PathReader.Static (PathReader)
import Effectful.FileSystem.PathReader.Static qualified as PR
import Effectful.FileSystem.PathWriter.Static qualified as PW
import Effectful.Optparse.Static qualified as EOA
import Effectful.Process (Process)
import Effectful.Process qualified as EProcess
import Effectful.State.Static.Shared qualified as SState
import Effectful.Terminal.Dynamic
  ( Terminal (GetTerminalSize),
    Window (Window, height, width),
  )
import FileSystem.OsPath
  ( OsPath,
    decodeLenient,
    decodeThrowM,
    osp,
    ospPathSep,
    unsafeDecode,
    (</>),
  )
import FileSystem.UTF8 qualified as UTF8
import GHC.Stack.Types (HasCallStack)
import System.Environment qualified as Env
import System.Environment.Guard (ExpectEnv (ExpectEnvSet), guardOrElse')
import System.Timeout qualified as TO
import Test.Tasty
  ( TestName,
    TestTree,
    testGroup,
  )
import Test.Tasty qualified as Tasty
import Test.Tasty.Golden (DeleteOutputFile (OnPass), goldenVsFileDiff)

main :: IO ()
main =
  Tasty.defaultMain $
    Tasty.localOption OnPass $
      (Tasty.withResource setup teardown tests)
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
testMonitor getTestArgs =
  testMonitorHelper
    getTestArgs
    [ospPathSep|testMonitor|]
    "Monitors build output"

testMonitorShortWindow :: IO TestArgs -> TestTree
testMonitorShortWindow getTestArgs =
  testMonitorHelper
    (modTestArgs <$> getTestArgs)
    [ospPathSep|testMonitorShortWindow|]
    "Monitors with short window"
  where
    modTestArgs testArgs =
      testArgs
        { mWindow =
            Just
              ( Window
                  { height = 12,
                    width = 80
                  }
              )
        }

testMonitorHelper :: IO TestArgs -> OsPath -> String -> TestTree
testMonitorHelper getTestArgs goldenName desc = goldenDiffCustom desc goldenFP actualFP $ do
  testArgs <- getTestArgs
  let buildOsPath = testArgs.tmpDir </> goldenName <> [osp|.txt|]
  buildFilePath <- decodeThrowM buildOsPath

  logs <-
    -- Remove duplicates to make tests a little more robust (these are
    -- non-deterministic, and sometimes we receive duplicates).
    -- Yes nub is O(n^2) but n is small and this preserves order.
    L.nub . L.reverse
      <$> runMonitorLogs getTestArgs buildOsPath (args buildFilePath)

  let buildOutput = L.filter isBuildOutput $ F.toList logs
      formatted = T.intercalate logsSep buildOutput

  writeActualFile $ tToBs formatted
  where
    args p =
      [ "--color",
        "false",
        "--file",
        p,
        "--period",
        "1"
      ]

    logsSep = "\n" <> T.replicate 80 "-" <> "\n"

    isBuildOutput = T.isPrefixOf "To Build:"

    actualFP = unsafeDecode actualOsP
    actualOsP = goldenBase <> [ospPathSep|.actual|]

    goldenFP = unsafeDecode $ goldenBase <> [ospPathSep|.golden|]

    goldenBase = [ospPathSep|test/functional/goldens|] </> goldenName

    writeActualFile :: ByteString -> IO ()
    writeActualFile = writeBS actualOsP

formatStatusTests :: TestTree
formatStatusTests =
  testGroup
    "formatStatus"
    [ testFormatNl,
      testFormatNlTrunc,
      testFormatInl,
      testFormatInlTrunc,
      testFormatLargeStart,
      testFormatLargeBuilding1,
      testFormatLargeBuilding2
    ]

testFormatNl :: TestTree
testFormatNl = testFormatStyle desc [osp|testFormatNl|] FormatNl
  where
    desc = "Formats with newlines"

testFormatNlTrunc :: TestTree
testFormatNlTrunc = testFormatStyle desc [osp|testFormatNlTrunc|] (FormatNlTrunc 15)
  where
    desc = "Formats with newlines and truncation"

testFormatInl :: TestTree
testFormatInl = testFormatStyle desc [osp|testFormatInl|] (FormatInl 25)
  where
    desc = "Formats with inline"

testFormatInlTrunc :: TestTree
testFormatInlTrunc = testFormatStyle desc [osp|testFormatInlTrunc|] (FormatInlTrunc 11 25)
  where
    desc = "Formats with inline and truncation"

testFormatStyle :: String -> OsPath -> FormatStyle -> TestTree
testFormatStyle desc fileName style = goldenDiffCustom desc goldenFP actualFP $ do
  let result = BuildStatus.formatStatusInit coloring style exampleStatus
  writeActualFile $ tToBs result
  where
    actualFP = unsafeDecode actualOsP
    actualOsP = goldenBase <> [ospPathSep|.actual|]

    goldenFP = unsafeDecode $ goldenBase <> [ospPathSep|.golden|]

    goldenBase = [ospPathSep|test/functional/goldens|] </> fileName

    writeActualFile :: ByteString -> IO ()
    writeActualFile = writeBS actualOsP

testFormatLargeStart :: TestTree
testFormatLargeStart =
  testFormatManual
    [ospPathSep|testFormatLargeStart|]
    [ospPathSep|example_large_start.txt|]

testFormatLargeBuilding1 :: TestTree
testFormatLargeBuilding1 =
  testFormatManual
    [ospPathSep|testFormatLargeBuilding1|]
    [ospPathSep|example_large_building_1.txt|]

testFormatLargeBuilding2 :: TestTree
testFormatLargeBuilding2 =
  testFormatManual
    [ospPathSep|testFormatLargeBuilding2|]
    [ospPathSep|example_large_building_2.txt|]

testFormatManual :: OsPath -> OsPath -> TestTree
testFormatManual goldenName inputName = goldenDiffCustom desc goldenFP actualFP $ do
  eResult <- runner $ do
    Monitor.readFormattedStatus
      coloring
      Nothing
      Nothing
      inputPath

  case eResult of
    Left err -> writeActualFile $ "Received error: " <> sToBs (show err)
    Right result -> writeActualFile $ tToBs result
  where
    desc = "Formats file with inline and truncation: " ++ show inputName

    actualFP = unsafeDecode actualOsP
    actualOsP = goldenBase <> [ospPathSep|.actual|]

    goldenFP = unsafeDecode $ goldenBase <> [ospPathSep|.golden|]

    goldenBase = [ospPathSep|test/functional/goldens|] </> goldenName

    inputPath = [ospPathSep|test/functional|] </> inputName

    writeActualFile :: ByteString -> IO ()
    writeActualFile = writeBS actualOsP

    runner =
      runEff
        . SState.evalState (Monitor.BuildWaiting, False)
        . PR.runPathReader
        . FR.runFileReader
        . runTerminalMock @Int (Just $ Window {height = 41, width = 174})

writeBS :: OsPath -> ByteString -> IO ()
writeBS actualOsP =
  runEff
    . FW.runFileWriter
    . FW.writeBinaryFile actualOsP
    . (<> "\n")

exampleStatus :: BuildStatusInit
exampleStatus =
  MkBuildStatus
    { toBuild = Set.fromList allPkgsL,
      building = Set.fromList (take 12 allPkgsL),
      completed = Set.fromList (take 5 allPkgsL)
    }
  where
    allPkgsL = fromString . ("lib" <>) . show @Int <$> [1 .. 20]

type Unit = ()

runMonitorLogs ::
  (HasCallStack) =>
  IO TestArgs -> OsPath -> [String] -> IO [Text]
runMonitorLogs getTestArgs buildFileOsPath cliArgs = do
  testArgs <- getTestArgs
  logsRef <- newIORef []
  _ <-
    TO.timeout
      -- Needs to be longer than however long it takes build.sh to finish
      -- printing all logs, so we have everything for the golden tests.
      14_000_000
      ( Env.withArgs cliArgs $ runner logsRef testArgs.mWindow $ do
          runBuildScript buildFileOsPath
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
  IORef [Text] ->
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
      -- I switched this from modifyIORef' as I thought this was causing a bug
      -- in the tests. That turned out to be false, but we should probably
      -- use this anyway as multiple threads are accessing this
      -- (timer thread and status printer).
      atomicModifyIORef' logsRef (\st -> (txt : st, ()))

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

  -- It seems we need shell and not proc, to run an actual script not on
  -- the PATH.
  void $ EProcess.createProcess $ EProcess.shell $ scriptPath <> " " <> outPath

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

coloring :: Coloring
coloring = MkColoring False

-- | We use a custom diff as this will print the actual diff to stdout,
-- whereas ordinary goldenVsFile will merely print something like
-- 'files are different'. The former is especially useful when we do not have
-- easy access to the diff files e.g. CI.
goldenDiffCustom :: TestName -> FilePath -> FilePath -> IO () -> TestTree
goldenDiffCustom x = goldenVsFileDiff x diffArgs
  where
    -- Apparently, the 'diff' program exists for windows and unix on CI. Thus
    -- the arguments ["diff", "-u" "--color=always", ref, new] also seem fine.
    -- Nevertheless, we use git as it is possibly more portable.
    diffArgs ref new =
      [ "git",
        "diff",
        "--exit-code",
        "--color=always",
        "--no-index",
        ref,
        new
      ]

sToBs :: String -> ByteString
sToBs = C8.pack

tToBs :: Text -> ByteString
tToBs = UTF8.encodeUtf8
