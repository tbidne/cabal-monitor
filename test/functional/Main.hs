{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Cabal.Monitor qualified as Monitor
import Cabal.Monitor.BuildStatus
  ( BuildStatus (MkBuildStatus, building, completed, toBuild),
    BuildStatusFinal,
    BuildStatusInit,
    FormatStyle
      ( FormatInl,
        FormatInlTrunc,
        FormatNl,
        FormatNlTrunc
      ),
  )
import Cabal.Monitor.BuildStatus qualified as BuildStatus
import Cabal.Monitor.Config
  ( Coloring (MkColoring),
    LocalPackages (MkLocalPackages),
    SearchInfix (MkSearchInfix),
  )
import Cabal.Monitor.Logger
  ( LogMode (LogModeFinish),
    RegionLogger
      ( DisplayRegions,
        LogRegion,
        WithRegion
      ),
  )
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C8
import Data.IORef (IORef)
import Data.IORef qualified as IORef
import Data.List qualified as L
import Data.Set qualified as Set
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as T
import Effectful (Eff, IOE, MonadIO (liftIO), runEff, type (:>))
import Effectful.Concurrent qualified as ECC
import Effectful.Concurrent.Async qualified as EAsync
import Effectful.Concurrent.Static qualified as CCS
import Effectful.Dispatch.Dynamic
  ( interpret,
    interpret_,
    localSeqUnlift,
    passthrough,
    reinterpret,
  )
import Effectful.Dynamic.Utils (ShowEffect (showEffectCons))
import Effectful.Environment.Static qualified as EEnv
import Effectful.FileSystem.FileReader.Static qualified as FR
import Effectful.FileSystem.FileWriter.Static qualified as FW
import Effectful.FileSystem.HandleReader.Static qualified as HR
import Effectful.FileSystem.HandleWriter.Static qualified as HW
import Effectful.FileSystem.PathReader.Dynamic (PathReader)
import Effectful.FileSystem.PathReader.Dynamic qualified as PR
import Effectful.FileSystem.PathWriter.Dynamic qualified as PW
import Effectful.Notify.Dynamic qualified as Notify
import Effectful.Optparse.Static qualified as EOA
import Effectful.Process qualified as EProcess
import Effectful.State.Static.Shared qualified as SState
import Effectful.Terminal.Dynamic
  ( Terminal (GetTerminalSize, PutStrLn),
    Window (Window, height, width),
  )
import FileSystem.OsPath (OsPath, osp, ospPathSep, (</>))
import FileSystem.OsPath qualified as OsP
import FileSystem.UTF8 qualified as UTF8
import GHC.Clock (getMonotonicTime)
import GHC.Stack.Types (HasCallStack)
import System.Environment.Guard (ExpectEnv (ExpectEnvSet), guardOrElse')
import System.Timeout qualified as TO
import Test.Tasty
  ( TestName,
    TestTree,
    testGroup,
  )
import Test.Tasty qualified as Tasty
import Test.Tasty.Golden (DeleteOutputFile (OnPass), goldenVsFileDiff)

#if !WINDOWS
import Control.Monad (void)
import Data.Foldable qualified as F
import Effectful.Process (Process)
import System.Environment qualified as Env
#endif

main :: IO ()
main =
  Tasty.defaultMain $
    Tasty.localOption OnPass (Tasty.withResource setup teardown tests)
  where
    tests getTestArgs =
      testGroup
        "Functional"
        $ [ formatStatusTests,
            testPidExit
          ]
          ++ monitorTests getTestArgs

-- NOTE: [Windows vs. posix newlines]
--
-- Getting the newlines correct here is tricky. Normally with golden tests,
-- we have a .gitattributes file with
--
--    *.golden -text
--
-- which seems to work to allow our tests without any special fanfare.
-- This wasn't straightforward here, though, perhaps because we are both
-- reading files created on linux and writing newlines ourselves.
--
-- What did work is NOT having that .gitattributes (i.e. it caused the tests
-- to fail) and also:
--
--   - Writing crlf on windows, lf on posix.
--   - Splitting newlines by crlf/lf on windows, lf on posix.

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
      testFormatLargeBuilding2,
      testFormatLargeBuilding3,
      testFormatLargeBuilding3Window,
      testFormatShortWindow,
      testFormatLocal,
      testFormatHeader1,
      testFormatHeaderLocal
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
    actualFP = OsP.unsafeDecode actualOsP
    actualOsP = goldenBase <> [ospPathSep|.actual|]

    goldenFP = OsP.unsafeDecode $ goldenBase <> [ospPathSep|.golden|]

    goldenBase = [ospPathSep|test/functional/goldens|] </> fileName <> osExt

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

testFormatLargeBuilding3 :: TestTree
testFormatLargeBuilding3 =
  testFormatManual
    [ospPathSep|testFormatLargeBuilding3|]
    [ospPathSep|example_large_building_3.txt|]

testFormatLargeBuilding3Window :: TestTree
testFormatLargeBuilding3Window =
  testFormatManualWindow
    (Window {height = 15, width = 150})
    [ospPathSep|testFormatLargeBuilding3Window|]
    [ospPathSep|example_large_building_3.txt|]

testFormatShortWindow :: TestTree
testFormatShortWindow =
  testFormatManualWindow
    (Window {height = 21, width = 174})
    [ospPathSep|testFormatShortWindow|]
    [ospPathSep|example_short.txt|]

testFormatLocal :: TestTree
testFormatLocal =
  testFormatManual
    [ospPathSep|testFormatLocal|]
    [ospPathSep|local.txt|]

testFormatHeader1 :: TestTree
testFormatHeader1 =
  testFormatManual
    [ospPathSep|testFormatHeader1|]
    [ospPathSep|header_1.txt|]

testFormatHeaderLocal :: TestTree
testFormatHeaderLocal =
  testFormatManual
    [ospPathSep|testFormatHeaderLocal|]
    [ospPathSep|header_local.txt|]

testFormatManual :: OsPath -> OsPath -> TestTree
testFormatManual = testFormatManualWindow $ Window {height = 41, width = 174}

testFormatManualWindow :: Window Int -> OsPath -> OsPath -> TestTree
testFormatManualWindow window goldenName inputName = goldenDiffCustom desc goldenFP actualFP $ do
  termRef <- IORef.newIORef []
  eResult <- runner termRef $ do
    style <- Monitor.mkFormatStyleFn Nothing Nothing
    Monitor.readFormattedStatus coloring localPackages searchInfix style inputPath

  case eResult of
    Left err -> writeActualFile $ "Received error: " <> sToBs (show err)
    Right result -> writeActualFile $ tToBs result
  where
    desc = "Formats file with inline and truncation: " ++ show inputName

    actualFP = OsP.unsafeDecode actualOsP
    actualOsP = goldenBase <> [ospPathSep|.actual|]

    goldenFP = OsP.unsafeDecode $ goldenBase <> [ospPathSep|.golden|]

    goldenBase = [ospPathSep|test/functional/goldens|] </> goldenName <> osExt

    inputPath = [ospPathSep|test/functional|] </> inputName

    writeActualFile :: ByteString -> IO ()
    writeActualFile = writeBS actualOsP

    runner termRef =
      runEff
        . SState.evalState (Monitor.BuildWaiting, mempty :: BuildStatusFinal)
        . runPathReader
        . FR.runFileReader
        . runTerminalMock @Int (Just window) termRef

testPidExit :: TestTree
testPidExit = goldenDiffCustom desc goldenFP actualFP $ do
  logsRef <- IORef.newIORef []
  termRef <- IORef.newIORef []
  processPidRef <- IORef.newIORef $ error "processPid not set"

  let (sleepCmd, sleepArgs) = shSleep 5
  let createProc = EProcess.proc sleepCmd sleepArgs
      process = EProcess.withCreateProcess createProc $ \_ _ _ ph -> do
        pid <-
          EProcess.getPid ph >>= \case
            Nothing -> error "Failed obtaining process pid"
            Just p -> pure p
        liftIO $ IORef.writeIORef processPidRef pid
        -- NOTE: It is absolutely critical that the functional test suite is
        -- built with -threaded, otherwise this blocks all threads i.e.
        -- test is broken!
        EProcess.waitForProcess ph

  start <- getMonotonicTime

  let action =
        process `EAsync.concurrently_` do
          -- Wait for sleep process to run
          CCS.sleep 1

          pid <- liftIO $ IORef.readIORef processPidRef
          let args = mkArgs pid

          EEnv.withArgs args $ Monitor.runMonitor Unit Notify.NotifyEnv

  -- This should finish around 5 seconds, but we include the timeout so we
  -- do not get an infinite loop in case of failure.
  TO.timeout 10_000_000
    . runner logsRef termRef
    $ action

  end <- getMonotonicTime

  let diff = round @_ @Int $ end - start
      timeSum =
        tToBs $
          if diff >= 4 && diff < 7
            then "Process took ~5 seconds as expected"
            else "Process took ~" <> showt diff <> " seconds, not expected ~5!"

  logs <- L.nub . L.reverse <$> IORef.readIORef logsRef
  term <- L.nub . L.reverse <$> IORef.readIORef termRef

  let formatted =
        T.intercalate logsSep
          . fmap massageExitLog
          . L.filter (\t -> isBuildOutput t || isExitLog t)
          $ logs <> term

  writeActualFile $ timeSum <> nl <> nl <> tToBs formatted
  where
    desc = "Exits with --pid"

    mkArgs pid =
      [ "--color",
        "off",
        "-f",
        inputPath,
        "--period",
        "1",
        "--pid",
        show pid
      ]

    runner logsRef termRef =
      runEff
        . ECC.runConcurrent
        . EEnv.runEnvironment
        -- Can use real runNotify since we are not sending anything in the
        -- tests.
        . Notify.runNotify
        . EProcess.runProcess
        . runTerminalMock @Int Nothing termRef
        -- By only checking 'finish' logs, we verify that the log has been
        -- printing as a final log at the end.
        . runRegionLoggerMock True logsRef
        . runPathReader
        . HW.runHandleWriter
        . HR.runHandleReader
        . FR.runFileReader
        . FW.runFileWriter
        . EOA.runOptparse

    actualOsP = goldenBase <> [osp|.actual|]
    actualFP = OsP.unsafeDecode actualOsP
    goldenFP = OsP.unsafeDecode $ goldenBase <> [osp|.golden|]

    goldenBase = [ospPathSep|test/functional/goldens/testPidExit|] <> osExt

    inputPath = OsP.unsafeDecode [ospPathSep|test/functional/example_short.txt|]

    writeActualFile :: ByteString -> IO ()
    writeActualFile = writeBS actualOsP

showt :: (Show a) => a -> Text
showt = T.pack . show

writeBS :: OsPath -> ByteString -> IO ()
writeBS actualOsP =
  runEff
    . FW.runFileWriter
    . FW.writeBinaryFile actualOsP
    . (<> nl)

exampleStatus :: BuildStatusInit
exampleStatus =
  MkBuildStatus
    { toBuild = Set.fromList allPkgsL,
      building = Set.fromList (take 12 allPkgsL),
      buildingLocal = mempty,
      completed = Set.fromList (take 5 allPkgsL)
    }
  where
    allPkgsL = fromString . ("lib" <>) . show @Int <$> [1 .. 20]

type Unit = ()

runRegionLoggerMock ::
  forall r es a.
  ( r ~ (),
    HasCallStack,
    IOE :> es
  ) =>
  -- | If true, only checks 'finish' logs.
  Bool ->
  IORef [Text] ->
  Eff (RegionLogger r : es) a ->
  Eff es a
runRegionLoggerMock finishOnly logsRef = interpret $ \env -> \case
  LogRegion logMode _ t ->
    case (finishOnly, logMode) of
      (True, LogModeFinish) -> writeLogs t
      (True, _) -> pure ()
      _ -> writeLogs t
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
      IORef.atomicModifyIORef' logsRef (\st -> (txt : st, ()))

runTerminalMock ::
  (Integral b, IOE :> es) =>
  Maybe (Window b) ->
  IORef [Text] ->
  Eff (Terminal : es) a ->
  Eff es a
runTerminalMock @_ @es mWindow termRef = interpret_ $ \case
  GetTerminalSize ->
    pure $ case mWindow of
      Just window ->
        Window
          { height = fromIntegral window.height,
            width = fromIntegral window.width
          }
      Nothing -> Window {height = 80, width = 100}
  PutStrLn s -> writeLogs $ T.pack s
  other -> error $ "runTerminalMock: " ++ showEffectCons other
  where
    writeLogs :: Text -> Eff es ()
    writeLogs txt = liftIO $ do
      IORef.atomicModifyIORef' termRef (\st -> (txt : st, ()))

data TestArgs = MkTestArgs
  { tmpDir :: OsPath,
    mWindow :: Maybe (Window Int)
  }

setup :: (HasCallStack) => IO TestArgs
setup = runTestEff $ do
  tmpDir <- (</> [ospPathSep|cabal-monitor|]) <$> PR.getTemporaryDirectory

  PW.removePathForciblyIfExists_ tmpDir

  PW.createDirectoryIfMissing True tmpDir

  pure $
    MkTestArgs
      { tmpDir,
        mWindow = Nothing
      }

teardown :: (HasCallStack) => TestArgs -> IO ()
teardown testArgs = do
  let cleanup = runTestEff . PW.removePathForciblyIfExists_ $ testArgs.tmpDir
      doNothing =
        putStrLn $
          "*** Not cleaning up tmp dir: '"
            <> OsP.decodeLenient testArgs.tmpDir
            <> "'"

  guardOrElse' "NO_CLEANUP" ExpectEnvSet doNothing cleanup

runTestEff ::
  (HasCallStack) =>
  Eff [PR.PathReader, PW.PathWriter, IOE] a -> IO a
runTestEff =
  runEff
    . PW.runPathWriter
    . runPathReader

coloring :: Coloring
coloring = MkColoring False

localPackages :: LocalPackages
localPackages = MkLocalPackages True

searchInfix :: SearchInfix
searchInfix = MkSearchInfix True

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

runPathReader :: (IOE :> es) => Eff (PathReader : es) a -> Eff es a
runPathReader = reinterpret PR.runPathReader $ \env -> \case
  PR.DoesFileExist p -> do
    let pStr = OsP.decodeLenient p
    if "xdg" `L.isInfixOf` pStr
      then pure False
      else PR.doesFileExist p
  PR.GetXdgDirectory _ _ -> pure [osp|xdg|]
  other -> passthrough env other

logsSep :: Text
logsSep = nl <> T.replicate 80 "-" <> nl

isBuildOutput :: Text -> Bool
isBuildOutput = T.isPrefixOf "To Build:"

isExitLog :: Text -> Bool
isExitLog = T.isInfixOf "terminating cabal-monitor"

massageExitLog :: Text -> Text
massageExitLog txt = case stripInfixTxt "Process with pid " txt of
  Nothing -> txt
  Just (pre, r1) ->
    let (_, r2) = T.break (== ' ') r1
     in mconcat
          [ pre,
            "Process with pid <pid>",
            r2
          ]

stripInfixTxt :: Text -> Text -> Maybe (Text, Text)
stripInfixTxt t1 t2 = (pre,) <$> T.stripPrefix t1 rest
  where
    (pre, rest) = T.breakOn t1 t2

shSleep :: Int -> (String, [String])
-- Previously this was ("timeout", ["/t", show n]) on windows, but sleep
-- seems to work fine.
shSleep n = ("sleep", [show n])

-- Common signatures
monitorTests :: IO TestArgs -> [TestTree]
nl :: (IsString s) => s
osExt :: OsPath

#if WINDOWS

nl = "\r\n"
osExt = [osp|_windows|]

monitorTests _ = []

#else

nl = "\n"
osExt = [osp|_posix|]

monitorTests getTestArgs =
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
  buildFilePath <- OsP.decodeThrowM buildOsPath

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
        "off",
        "--file",
        p,
        "--period",
        "1"
      ]

    actualFP = OsP.unsafeDecode actualOsP
    actualOsP = goldenBase <> [ospPathSep|.actual|]

    goldenFP = OsP.unsafeDecode $ goldenBase <> [ospPathSep|.golden|]

    goldenBase = [ospPathSep|test/functional/goldens|] </> goldenName

    writeActualFile :: ByteString -> IO ()
    writeActualFile = writeBS actualOsP

runMonitorLogs ::
  (HasCallStack) =>
  IO TestArgs ->
  OsPath ->
  [String] ->
  IO [Text]
runMonitorLogs getTestArgs buildFileOsPath cliArgs = do
  testArgs <- getTestArgs
  logsRef <- IORef.newIORef []
  termRef <- IORef.newIORef []
  _ <-
    TO.timeout
      -- Needs to be longer than however long it takes build.sh to finish
      -- printing all logs, so we have everything for the golden tests.
      14_000_000
      ( Env.withArgs cliArgs $ runner logsRef termRef testArgs.mWindow $ do
          runBuildScript buildFileOsPath
            `EAsync.concurrently`
            -- start this second so build file exists.
            (ECC.threadDelay 500_000 *> Monitor.runMonitor Unit Notify.NotifyEnv)
      )
  IORef.readIORef logsRef
  where
    runner logsRef termRef mWindow =
      runEff
        . ECC.runConcurrent
        . EProcess.runProcess
        . Notify.runNotify
        . runTerminalMock mWindow termRef
        . runRegionLoggerMock False logsRef
        . runPathReader
        . HW.runHandleWriter
        . HR.runHandleReader
        . FR.runFileReader
        . FW.runFileWriter
        . EOA.runOptparse

runBuildScript ::
  ( HasCallStack,
    PathReader :> es,
    Process :> es
  ) =>
  OsPath ->
  Eff es ()
runBuildScript buildFileOsPath = do
  pwd <- PR.getCurrentDirectory
  scriptPath <- OsP.decodeThrowM (pwd </> [ospPathSep|test/functional/build.sh|])

  outPath <- OsP.decodeThrowM buildFileOsPath

  -- It seems we need shell and not proc, to run an actual script not on
  -- the PATH.
  void $ EProcess.createProcess $ EProcess.shell $ scriptPath <> " " <> outPath

#endif
