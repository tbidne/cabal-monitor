{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C8
import Effectful (Eff, IOE, runEff)
import Effectful.FileSystem.FileReader.Static qualified as FR
import FileSystem.OsPath (OsPath, ospPathSep)
import GHC.Stack (HasCallStack)
import Monitor (Status)
import Monitor qualified
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
    [ env (pure sampleLines) benchParseStatus,
      env (pure sampleStatus) benchFormatStatus,
      benchReadFormatted samplePath
    ]

benchParseStatus :: [ByteString] -> Benchmark
benchParseStatus txtLines =
  bench "parseStatus" $ nf Monitor.parseStatus txtLines

benchFormatStatus :: Status -> Benchmark
benchFormatStatus status =
  bench "formatStatus" $ nf Monitor.formatStatus status

benchReadFormatted :: OsPath -> Benchmark
benchReadFormatted path =
  bench "readFormattedStatus" $
    nfIO (runBenchEff . Monitor.readFormattedStatus $ path)

samplePath :: OsPath
samplePath = [ospPathSep|./bench/sample.txt|]

sampleBS :: ByteString
sampleBS = $$TH.readSampleTH

sampleLines :: [ByteString]
sampleLines = C8.lines sampleBS

sampleStatus :: Status
sampleStatus = Monitor.parseStatus sampleLines

runBenchEff ::
  (HasCallStack) =>
  Eff [FR.FileReader, IOE] a ->
  IO a
runBenchEff = runEff . FR.runFileReader
