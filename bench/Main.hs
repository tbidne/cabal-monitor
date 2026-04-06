{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Cabal.Monitor (BuildState (Building))
import Cabal.Monitor qualified as Monitor
import Cabal.Monitor.BuildStatus
  ( BuildStatusFinal,
    BuildStatusInit,
    FormatStyle (FormatInlTrunc, FormatNl),
  )
import Cabal.Monitor.BuildStatus qualified as Status
import Cabal.Monitor.Config
  ( Coloring (MkColoring),
    Height,
    LocalPackages (MkLocalPackages),
    SearchInfix (MkSearchInfix),
    Width,
  )
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C8
import Data.List qualified as L
import Effectful (Eff, IOE, runEff, (:>))
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Dispatch.Dynamic (passthrough, reinterpret)
import Effectful.FileSystem.FileReader.Static qualified as FR
import Effectful.FileSystem.PathReader.Dynamic (PathReader)
import Effectful.FileSystem.PathReader.Dynamic qualified as PR
import Effectful.State.Static.Shared qualified as SState
import Effectful.Terminal.Dynamic qualified as Term
import FileSystem.OsPath (OsPath, decodeLenient, osp, ospPathSep)
import GHC.Stack (HasCallStack)
import TH qualified
import Test.Tasty.Bench
  ( Benchmark,
    bench,
    bgroup,
    defaultMain,
    env,
    nf,
    nfIO,
  )

main :: IO ()
main = do
  defaultSyleFn <- mkStyleFn Nothing Nothing
  compactSyleFn <- mkStyleFn (Just termHeight) (Just termWidth)

  defaultMain $
    benchParsers
      ++ [ env (pure sampleStatus) benchFormatStatus,
           env (pure sampleStatus) benchFormatStatusCompact,
           benchReadFormatted defaultSyleFn samplePath,
           benchReadFormattedCompact compactSyleFn samplePath
         ]

benchParsers :: [Benchmark]
benchParsers =
  [ bgroup "simple" (bsParsers sampleBS),
    bgroup "local" (bsParsers localBS),
    bgroup "infix" (bsParsers sampleInfixBS),
    bgroup "local_infix" (bsParsers localInfixBS)
  ]

bsParsers :: ByteString -> [Benchmark]
bsParsers bs =
  [ env (pure bs) benchParseStatus,
    env (pure bs) benchParseStatusLocal,
    env (pure bs) benchParseStatusInfix,
    env (pure bs) benchParseStatusLocalInfix
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

mkStyleFn :: Maybe Height -> Maybe Width -> IO (BuildStatusFinal -> FormatStyle)
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

termHeight :: Height
termHeight = 25

termWidth :: Width
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
      SState.State (BuildState, BuildStatusFinal),
      Concurrent,
      IOE
    ]
    a ->
  IO a
runBenchEff m = runner
  where
    runner = runEff $ runConcurrent $ do
      SState.evalState (Building, mempty :: BuildStatusFinal)
        . Term.runTerminal
        . runPathReader
        . FR.runFileReader
        $ m

runPathReader :: (IOE :> es) => Eff (PathReader : es) a -> Eff es a
runPathReader = reinterpret PR.runPathReader $ \env' -> \case
  PR.DoesFileExist p -> do
    let pStr = decodeLenient p
    if "xdg" `L.isInfixOf` pStr
      then pure False
      else PR.doesFileExist p
  PR.GetXdgDirectory _ _ -> pure [osp|xdg|]
  other -> passthrough env' other
