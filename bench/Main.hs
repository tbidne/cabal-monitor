{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Data.Text (Text)
import Effectful (Eff, IOE, runEff)
import GHC.Stack (HasCallStack)
import Data.Text qualified as T
import Effectful.FileSystem.FileReader.Static qualified as FR
import FileSystem.OsPath (OsPath, ospPathSep)
import FileSystem.UTF8 qualified as UTF8
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)
import Monitor (Status)
import Monitor qualified
import Test.Tasty.Bench
  ( Benchmark,
    bench,
    env,
    defaultMain,
    nf,
    nfIO,
  )

main :: IO ()
main = do
  defaultMain
    [ env sampleLines benchParseStatus,
      env sampleStatus benchFormatStatus,
      benchReadFormatted samplePath
    ]

benchParseStatus :: [Text] -> Benchmark
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

-- TODO: Should probably make these pure

sampleLines :: IO [Text]
sampleLines = runBenchEff $ do
  contents <- UTF8.decodeUtf8ThrowM =<< FR.readBinaryFile samplePath
  pure $ T.lines contents

sampleStatus :: HasCallStack => IO Status
sampleStatus = runBenchEff $ do
  contents <- UTF8.decodeUtf8ThrowM =<< FR.readBinaryFile samplePath
  let txtLines = T.lines contents
      status = Monitor.parseStatus txtLines
  pure status

runBenchEff ::
  (HasCallStack) =>
  Eff [FR.FileReader, IOE] a ->
  IO a
runBenchEff = runEff . FR.runFileReader
