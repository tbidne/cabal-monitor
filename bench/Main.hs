{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Cabal.Monitor (BuildState (Building))
import Cabal.Monitor qualified as Monitor
import Cabal.Monitor.Args (Coloring (MkColoring), SearchInfix (MkSearchInfix))
import Cabal.Monitor.BuildStatus
  ( BuildStatusInit,
    FormatStyle (FormatInlTrunc, FormatNl),
  )
import Cabal.Monitor.BuildStatus qualified as Status
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C8
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
  defaultSyleFn <- mkStyleFn Nothing Nothing
  compactSyleFn <- mkStyleFn (Just termHeight) (Just termWidth)

  defaultMain
    [ env (pure sampleBS) benchParseStatus,
      env (pure sampleBSInfix) benchParseStatusInfix,
      env (pure sampleStatus) benchFormatStatus,
      env (pure sampleStatus) benchFormatStatusCompact,
      benchReadFormatted defaultSyleFn samplePath,
      benchReadFormattedCompact compactSyleFn samplePath
    ]

benchParseStatus :: ByteString -> Benchmark
benchParseStatus txtLines =
  bench "parseStatus" $ nf Status.parseStatus txtLines

benchParseStatusInfix :: ByteString -> Benchmark
benchParseStatusInfix txtLines =
  bench "parseStatusInfix" $ nf Status.parseStatusInfix txtLines

benchFormatStatus :: BuildStatusInit -> Benchmark
benchFormatStatus status =
  bench "formatStatus" $ nf (Status.formatStatusInit coloring FormatNl) status

benchFormatStatusCompact :: BuildStatusInit -> Benchmark
benchFormatStatusCompact status =
  bench "formatStatus_compact" $ nf (Status.formatStatusInit coloring compactStyle) status

benchReadFormatted :: (BuildStatusInit -> FormatStyle) -> OsPath -> Benchmark
benchReadFormatted styleFn path =
  bench "readFormattedStatus" $
    nfIO (runBenchEff . Monitor.readFormattedStatus coloring searchInfix styleFn $ path)

benchReadFormattedCompact :: (BuildStatusInit -> FormatStyle) -> OsPath -> Benchmark
benchReadFormattedCompact styleFn path =
  bench "readFormattedStatus_compact" $
    nfIO
      ( runBenchEff
          . Monitor.readFormattedStatus coloring searchInfix styleFn
          $ path
      )

mkStyleFn :: Maybe Natural -> Maybe Natural -> IO (BuildStatusInit -> FormatStyle)
mkStyleFn mHeight mWidth =
  runEff
    . Term.runTerminal
    $ Monitor.mkFormatStyleFn mHeight mWidth

compactStyle :: FormatStyle
compactStyle = FormatInlTrunc termHeight termWidth

coloring :: Coloring
coloring = MkColoring False

searchInfix :: SearchInfix
searchInfix = MkSearchInfix False

termHeight :: Natural
termHeight = 25

termWidth :: Natural
termWidth = 80

samplePath :: OsPath
samplePath = [ospPathSep|./bench/sample.txt|]

sampleBS :: ByteString
sampleBS = $$TH.readSampleTH

sampleBSInfix :: ByteString
sampleBSInfix =
  C8.unlines
    . fmap (pfx <>)
    . C8.lines
    $ sampleBS
  where
    pfx = "   Some prefix   "

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
