{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Cabal.Monitor (BuildState (Building))
import Cabal.Monitor qualified as Monitor
import Cabal.Monitor.BuildStatus
  ( BuildStatusInit,
    FormatStyle (FormatInlTrunc, FormatNl),
  )
import Cabal.Monitor.BuildStatus qualified as Status
import Data.ByteString (ByteString)
import Effectful (Eff, IOE, runEff)
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.FileSystem.FileReader.Static qualified as FR
import Effectful.FileSystem.PathReader.Static qualified as PR
import Effectful.State.Static.Shared qualified as SState
import Effectful.Terminal.Dynamic qualified as Term
import FileSystem.OsPath (OsPath, ospPathSep)
import GHC.Stack (HasCallStack)
import Numeric.Natural (Natural)
import TH qualified
import Test.Tasty.Bench
  ( Benchmark,
    bench,
    defaultMain,
    env,
    nf,
    nfIO,
  )

main :: IO ()
main = do
  defaultMain
    [ env (pure sampleBS) benchParseStatus,
      env (pure sampleStatus) benchFormatStatus,
      env (pure sampleStatus) benchFormatStatusCompact,
      benchReadFormatted samplePath,
      benchReadFormattedCompact samplePath
    ]

benchParseStatus :: ByteString -> Benchmark
benchParseStatus txtLines =
  bench "parseStatus" $ nf Status.parseStatus txtLines

benchFormatStatus :: BuildStatusInit -> Benchmark
benchFormatStatus status =
  bench "formatStatus" $ nf (Status.formatStatusInit FormatNl) status

benchFormatStatusCompact :: BuildStatusInit -> Benchmark
benchFormatStatusCompact status =
  bench "formatStatus_compact" $ nf (Status.formatStatusInit compactStyle) status

benchReadFormatted :: OsPath -> Benchmark
benchReadFormatted path =
  bench "readFormattedStatus" $
    nfIO (runBenchEff . Monitor.readFormattedStatus Nothing Nothing $ path)

benchReadFormattedCompact :: OsPath -> Benchmark
benchReadFormattedCompact path =
  bench "readFormattedStatus_compact" $
    nfIO
      ( runBenchEff
          . Monitor.readFormattedStatus (Just termHeight) (Just termWidth)
          $ path
      )

compactStyle :: FormatStyle
compactStyle = FormatInlTrunc termHeight termWidth

termHeight :: Natural
termHeight = 25

termWidth :: Natural
termWidth = 80

samplePath :: OsPath
samplePath = [ospPathSep|./bench/sample.txt|]

sampleBS :: ByteString
sampleBS = $$TH.readSampleTH

sampleStatus :: BuildStatusInit
sampleStatus = Status.parseStatus sampleBS

runBenchEff ::
  (HasCallStack) =>
  Eff
    [ FR.FileReader,
      PR.PathReader,
      Term.Terminal,
      SState.State (BuildState, Bool),
      Concurrent,
      IOE
    ]
    a ->
  IO a
runBenchEff m = runner
  where
    runner = runEff $ runConcurrent $ do
      SState.evalState (Building, False)
        . Term.runTerminal
        . PR.runPathReader
        . FR.runFileReader
        $ m
