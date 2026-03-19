{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Cabal.Monitor (BuildState (Building))
import Cabal.Monitor qualified as Monitor
import Cabal.Monitor.Args
  ( Coloring (MkColoring),
    LocalPackages (MkLocalPackages),
    SearchInfix (MkSearchInfix),
  )
import Cabal.Monitor.BuildStatus
  ( BuildStatusInit,
    BuildStatusFinal,
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
      env (pure localBS) benchParseStatusLocal,
      env (pure sampleInfixBS) benchParseStatusInfix,
      env (pure localInfixBS) benchParseStatusLocalInfix,
      env (pure sampleStatus) benchFormatStatus,
      env (pure sampleStatus) benchFormatStatusCompact,
      benchReadFormatted defaultSyleFn samplePath,
      benchReadFormattedCompact compactSyleFn samplePath
    ]

benchParseStatus :: ByteString -> Benchmark
benchParseStatus txtLines =
  bench "parseStatus" $ nf (Status.parseStatus localPackages searchInfix) txtLines

benchParseStatusLocal :: ByteString -> Benchmark
benchParseStatusLocal txtLines =
  bench "parseStatusLocal" $ nf (Status.parseStatus (MkLocalPackages True) searchInfix) txtLines

benchParseStatusInfix :: ByteString -> Benchmark
benchParseStatusInfix txtLines =
  bench "parseStatusInfix" $ nf (Status.parseStatus localPackages (MkSearchInfix True)) txtLines

benchParseStatusLocalInfix :: ByteString -> Benchmark
benchParseStatusLocalInfix txtLines =
  bench "parseStatusLocalInfix" $ nf (Status.parseStatus (MkLocalPackages True) (MkSearchInfix True)) txtLines

benchFormatStatus :: BuildStatusInit -> Benchmark
benchFormatStatus status =
  bench "formatStatus" $ nf (Status.formatStatusInit coloring FormatNl) status

benchFormatStatusCompact :: BuildStatusInit -> Benchmark
benchFormatStatusCompact status =
  bench "formatStatus_compact" $ nf (Status.formatStatusInit coloring compactStyle) status

benchReadFormatted :: (BuildStatusFinal -> FormatStyle) -> OsPath -> Benchmark
benchReadFormatted styleFn path =
  bench "readFormattedStatus" $
    nfIO (runBenchEff . Monitor.readFormattedStatus coloring localPackages searchInfix styleFn $ path)

benchReadFormattedCompact :: (BuildStatusFinal -> FormatStyle) -> OsPath -> Benchmark
benchReadFormattedCompact styleFn path =
  bench "readFormattedStatus_compact" $
    nfIO
      ( runBenchEff
          . Monitor.readFormattedStatus coloring localPackages searchInfix styleFn
          $ path
      )

mkStyleFn :: Maybe Natural -> Maybe Natural -> IO (BuildStatusFinal -> FormatStyle)
mkStyleFn mHeight mWidth =
  runEff
    . Term.runTerminal
    $ Monitor.mkFormatStyleFn mHeight mWidth

compactStyle :: FormatStyle
compactStyle = FormatInlTrunc termHeight termWidth

coloring :: Coloring
coloring = MkColoring False

localPackages :: LocalPackages
localPackages = MkLocalPackages False

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

sampleInfixBS :: ByteString
sampleInfixBS =
  C8.unlines
    . fmap (pfx <>)
    . C8.lines
    $ sampleBS
  where
    pfx = "   Some prefix   "

localBS :: ByteString
localBS = $$TH.readLocalPackagesTH

localInfixBS :: ByteString
localInfixBS = $$TH.readLocalPackagesInfixTH

sampleStatus :: BuildStatusInit
sampleStatus = Status.parseStatus localPackages searchInfix sampleBS

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
      SState.evalState (Building mempty, False)
        . Term.runTerminal
        . PR.runPathReader
        . FR.runFileReader
        $ m
