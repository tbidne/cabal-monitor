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
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
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
import Effectful.State.Static.Shared qualified as SState
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
import Test.Tasty.HUnit
  ( HasCallStack,
    assertBool,
    assertFailure,
    testCase,
    (@=?),
  )

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
testMonitor getTestArgs =
  testMonitorHelper
    getTestArgs
    [ospPathSep|testMonitor.txt|]
    "Monitors build output"
    expected
  where
    expected = [e0, e1, e2, e3, e4, e5, t0, t1]

    t0 = "Waiting to start:"

    t1 = "Building: 1 second"

    -- NOTE: No "Finished in" log because its presence is non-deterministic
    -- (based on timing).

    e0 = "Build file does not exist at path:"

    e1 =
      unlineStrip
        [ Pretty.magenta <> "To Build: 8",
          "  - aeson-0.7",
          "  - bits-0.6",
          "  - byteable-0.1.1",
          "  - indexed-profunctors-0.1.1.1",
          "  - lens-1",
          "  - mtl-compat-0.2.2",
          "  - profunctors-2",
          "  - string-qq-0.0.6" <> Pretty.endCode,
          "",
          Pretty.yellow <> "Building: 0" <> Pretty.endCode,
          "",
          Pretty.green <> "Completed: 0" <> Pretty.endCode
        ]
    e2 =
      unlineStrip
        [ Pretty.magenta <> "To Build: 4",
          "  - lens-1",
          "  - mtl-compat-0.2.2",
          "  - profunctors-2",
          "  - string-qq-0.0.6" <> Pretty.endCode,
          "",
          Pretty.yellow <> "Building: 4",
          "  - aeson-0.7",
          "  - bits-0.6",
          "  - byteable-0.1.1",
          "  - indexed-profunctors-0.1.1.1" <> Pretty.endCode,
          "",
          Pretty.green <> "Completed: 0" <> Pretty.endCode
        ]

    e3 =
      unlineStrip
        [ Pretty.magenta <> "To Build: 2",
          "  - profunctors-2",
          "  - string-qq-0.0.6" <> Pretty.endCode,
          "",
          Pretty.yellow <> "Building: 5",
          "  - bits-0.6",
          "  - byteable-0.1.1",
          "  - indexed-profunctors-0.1.1.1",
          "  - lens-1",
          "  - mtl-compat-0.2.2" <> Pretty.endCode,
          "",
          Pretty.green <> "Completed: 1",
          "  - aeson-0.7" <> Pretty.endCode
        ]

    e4 =
      unlineStrip
        [ Pretty.magenta <> "To Build: 1",
          "  - string-qq-0.0.6" <> Pretty.endCode,
          "",
          Pretty.yellow <> "Building: 4",
          "  - indexed-profunctors-0.1.1.1",
          "  - lens-1",
          "  - mtl-compat-0.2.2",
          "  - profunctors-2" <> Pretty.endCode,
          "",
          Pretty.green <> "Completed: 3",
          "  - aeson-0.7",
          "  - bits-0.6",
          "  - byteable-0.1.1" <> Pretty.endCode
        ]

    e5 =
      unlineStrip
        [ Pretty.magenta <> "To Build: 0" <> Pretty.endCode,
          "",
          Pretty.yellow <> "Building: 3",
          "  - mtl-compat-0.2.2",
          "  - profunctors-2",
          "  - string-qq-0.0.6" <> Pretty.endCode,
          "",
          Pretty.green <> "Completed: 5",
          "  - aeson-0.7",
          "  - bits-0.6",
          "  - byteable-0.1.1",
          "  - indexed-profunctors-0.1.1.1",
          "  - lens-1" <> Pretty.endCode
        ]

testMonitorShortWindow :: IO TestArgs -> TestTree
testMonitorShortWindow getTestArgs =
  testMonitorHelper
    (modTestArgs <$> getTestArgs)
    [ospPathSep|testMonitorShortWindow.txt|]
    "Monitors with short window"
    expected
  where
    modTestArgs testArgs =
      testArgs
        { mWindow =
            Just
              ( Window
                  { height = 16,
                    width = 80
                  }
              )
        }

    expected = [e0, e1, e2, e3, e4, e5, t0, t1]

    t0 = "Waiting to start:"

    t1 = "Building: 1 second"

    e0 = "Build file does not exist at path:"

    e1 =
      unlineStrip
        [ Pretty.magenta <> "To Build: 8",
          "  - aeson-0.7, bits-0.6, byteable-0.1.1, indexed-profunctors-0.1.1.1, lens-1",
          "  - mtl-compat-0.2.2, profunctors-2, string-qq-0.0.6" <> Pretty.endCode,
          "",
          Pretty.yellow <> "Building: 0" <> Pretty.endCode,
          "",
          Pretty.green <> "Completed: 0" <> Pretty.endCode
        ]
    e2 =
      unlineStrip
        [ Pretty.magenta <> "To Build: 4",
          "  - lens-1, mtl-compat-0.2.2, profunctors-2, string-qq-0.0.6" <> Pretty.endCode,
          "",
          Pretty.yellow <> "Building: 4",
          "  - aeson-0.7, bits-0.6, byteable-0.1.1, indexed-profunctors-0.1.1.1" <> Pretty.endCode,
          "",
          Pretty.green <> "Completed: 0" <> Pretty.endCode
        ]

    e3 =
      unlineStrip
        [ Pretty.magenta <> "To Build: 2",
          "  - profunctors-2, string-qq-0.0.6" <> Pretty.endCode,
          "",
          Pretty.yellow <> "Building: 5",
          "  - bits-0.6, byteable-0.1.1, indexed-profunctors-0.1.1.1, lens-1",
          "  - mtl-compat-0.2.2" <> Pretty.endCode,
          "",
          Pretty.green <> "Completed: 1",
          "  - aeson-0.7" <> Pretty.endCode
        ]

    e4 =
      unlineStrip
        [ Pretty.magenta <> "To Build: 1",
          "  - string-qq-0.0.6" <> Pretty.endCode,
          "",
          Pretty.yellow <> "Building: 4",
          "  - indexed-profunctors-0.1.1.1, lens-1, mtl-compat-0.2.2, profunctors-2" <> Pretty.endCode,
          "",
          Pretty.green <> "Completed: 3",
          "  - aeson-0.7, bits-0.6, byteable-0.1.1" <> Pretty.endCode
        ]

    e5 =
      unlineStrip
        [ Pretty.magenta <> "To Build: 0" <> Pretty.endCode,
          "",
          Pretty.yellow <> "Building: 3",
          "  - mtl-compat-0.2.2, profunctors-2, string-qq-0.0.6" <> Pretty.endCode,
          "",
          Pretty.green <> "Completed: 5",
          "  - aeson-0.7, bits-0.6, byteable-0.1.1, indexed-profunctors-0.1.1.1, lens-1" <> Pretty.endCode
        ]

testMonitorHelper :: IO TestArgs -> OsPath -> String -> [Text] -> TestTree
testMonitorHelper getTestArgs fileName desc expected = testCase desc $ do
  testArgs <- getTestArgs
  let buildOsPath = testArgs.tmpDir </> fileName
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
                  T.intercalate logsSep (Set.toList logs)
                ]
      assertFailure msg
  where
    args p =
      [ "--file",
        p,
        "--period",
        "1"
      ]

    logsSep = "\n" <> T.replicate 80 "-" <> "\n"

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

testFormatLargeStart :: TestTree
testFormatLargeStart = testFormatManual expected [ospPathSep|example_large_start.txt|]
  where
    expected =
      [ Pretty.magenta <> "To Build: 359",
        "  - BNFC-2.9.6.1, BNFC-meta-0.6.1, BiobaseNewick-0.0.0.2, Blammo-2.1.3.0, Boolean-0.2.4, BoundedChan-1.0.3.0, ChannelT-0.0.0.7, Chart-1.9.5, Chart-cairo-1.9.4.1",
        "  - Chart-diagrams-1.9.5.1, ChasingBottoms-1.3.1.16, ClustalParser-1.3.0, Color-0.4.0, DAV-1.3.4, DPutils-0.1.1.0, Decimal-0.5.2, Diff-1.0.2, ENIG-0.0.1.0, Earley-0.13.0.1",
        "  - FailT-0.1.2.0, FenwickTree-0.1.2.1, FindBin-0.0.5, FloatingHex-0.5, FontyFruity-0.5.3.5, ForestStructures-0.0.1.1, GLFW-b-3.3.9.1, GLURaw-2.0.0.5, GLUT-2.7.0.16",
        "  - GenericPretty-1.2.2, Glob-0.10.2, HCodecs-0.5.2, HPDF-1.8, HSet-0.0.2, HSlippyMap-3.0.1, HStringTemplate-0.8.8, HSvm-1.0.3.35, HTF-0.15.0.2, HTTP-4000.4.1",
        "  - HUnit-1.6.2.0, HUnit-approx-1.1.1.1, HaTeX-3.23.0.1, HaXml-1.25.14, HandsomeSoup-0.4.2, HasBigDecimal-0.2.0.0, HaskellNet-0.6.2, HaskellNet-SSL-0.4.0.2, Hclip-3.0.0.4",
        "  - HsYAML-0.2.1.5, HsYAML-aeson-0.2.0.2, IPv6Addr-2.0.6.1, IPv6DB-0.3.3.4, IfElse-0.85, ImpSpec-0.1.0.0, Interpolation-0.3.0, IntervalMap-0.6.2.1, JuicyCairo-0.1.0.1",
        "  - JuicyPixels-3.3.9, JuicyPixels-extra-0.6.0, LPFP-1.1.5, LPFP-core-1.1.5, LetsBeRational-1.0.0.0, List-0.6.2, ListLike-4.7.8.4, ListTree-0.2.3, ListZipper-1.2.0.2",
        "  - Mantissa-0.1.0.0, MemoTrie-0.6.11, MissingH-1.6.0.2, MonadPrompt-1.0.0.5, MonadRandom-0.6.2, MultipletCombiner-0.0.7, MusicBrainz-0.4.1, NanoID-3.4.1.1, NoHoed-0.1.1",
        "  - NumInstances-1.4, ObjectName-1.1.0.2, OneTuple-0.4.2, Only-0.1, OpenAL-1.7.0.5, OpenGL-3.0.3.0, OpenGLRaw-3.3.4.1, PSQueue-1.2.2, ParsecTools-0.0.2.0, Plural-0.0.2",
        "  - PyF-0.11.4.0, QuasiText-0.1.2.6, QuickCheck-2.16.0.0, RSA-2.4.1, Ranged-sets-0.5.0, Rasterific-0.7.5.4, RefSerialize-0.4.0, RoundingFiasco-0.1.0.0, SHA-1.6.4.4",
        "  - STMonadTrans-0.4.8.1, SVGFonts-1.8.1, SafeSemaphore-0.10.1, SciBaseTypes-0.1.1.0, ShellCheck-0.11.0, Sit-0.2023.8.3, StateVar-1.2.2, Vis-1.0.0, active-0.2.1",
        "  - adjunctions-4.4.3, aeson-2.2.3.0, alex-3.5.4.0, alex-meta-0.3.0.13, ansi-terminal-1.1.3, ansi-terminal-types-1.1.3, appar-0.1.8, asn1-encoding-0.9.6, asn1-parse-0.9.5",
        "  - asn1-types-0.3.4, assoc-1.1.1, async-2.2.5, attoparsec-0.14.4, attoparsec-aeson-2.2.2.0, auto-update-0.2.6, base-orphans-0.9.3, base16-bytestring-1.0.2.0, base64-1.0",
        "  - base64-bytestring-1.2.1.0, basement-0.0.16, bibtex-0.1.0.7, bifunctors-5.6.2, binary-orphans-1.0.5, bindings-DSL-1.0.25, bindings-GLFW-3.3.9.2, bitvec-1.1.5.0",
        "  - blaze-builder-0.4.4.1, blaze-html-0.9.2.0, blaze-markup-0.8.3.0, blaze-svg-0.3.7, bmp-1.2.6.4, byteorder-1.0.4, bytes-0.17.4, bytestring-lexing-0.5.0.15, c-enum-0.1.1.3",
        "  - cabal-doctest-1.0.11, cairo-0.13.12.0, cairo-image-0.1.0.5, call-stack-0.4.0, case-insensitive-1.2.1.0, cborg-0.2.10.0, cereal-0.5.8.3, cereal-text-0.1.0.2",
        "  - cereal-vector-0.2.0.1, character-ps-0.1, clock-0.8.4, colour-2.3.6, comonad-5.0.9, conduit-1.3.6.1, conduit-extra-1.3.8, context-0.2.1.1, contravariant-1.5.5",
        "  - cookie-0.5.1, cpphs-1.20.10, crypto-api-0.13.3, crypto-pubkey-types-0.4.3, cryptohash-md5-0.11.101.0, crypton-1.0.4, crypton-connection-0.4.5, crypton-socks-0.6.2",
        "  - crypton-x509-1.7.7, crypton-x509-store-1.6.11, crypton-x509-system-1.6.7, crypton-x509-validation-1.6.14, data-accessor-0.2.3.1, data-accessor-transformers-0.2.1.8",
        "  - data-default-0.8.0.1, data-default-class-0.2.0.0, data-fix-0.3.4, data-lens-light-0.1.2.4, diagrams-cairo-1.5, diagrams-core-1.5.1.1, diagrams-lib-1.5.0.1",
        "  - diagrams-postscript-1.5.3, diagrams-solve-0.1.3.1, diagrams-svg-1.5, distributive-0.6.2.1, dlist-1.0, dual-tree-0.2.3.1, easy-file-0.2.5, ech-config-0.0.1",
        "  - entropy-0.4.1.11, envparse-0.6.0, errors-2.3.0, extra-1.8, fail-4.9.0.0, fast-logger-3.2.6, fgl-5.8.3.0, file-embed-0.0.16.0, fingertree-0.1.6.2, fixed-0.3",
        "  - fmlist-0.9.4, free-5.2, generated-0.1.0.0, generically-0.1.1, glib-0.13.12.0, gloss-1.13.2.2, gloss-rendering-1.13.2.1, gnuplot-0.5.7, groups-0.5.3",
        "  - gtk2hs-buildtools-0.13.12.0, half-0.3.3, happy-2.1.7, happy-lib-2.1.7, happy-meta-0.2.1.0, hashable-1.5.0.0, hashtables-1.4.2, haskell-lexer-1.2.1, haskell-src-1.0.4.2",
        "  - haskell-src-exts-1.23.1, haskell-src-meta-0.8.15, hedis-0.15.2, hourglass-0.2.12, hpke-0.0.0, hsc2hs-0.68.10, hslogger-1.3.2.0, hspec-2.11.13, hspec-core-2.11.13",
        "  - hspec-discover-2.11.13, hspec-expectations-0.8.4, hspec-expectations-lifted-0.10.0, http-client-0.7.19, http-client-tls-0.3.6.4, http-conduit-2.3.9.1, http-types-0.12.4",
        "  - hxt-9.3.1.22, hxt-charproperties-9.5.0.0, hxt-http-9.1.5.2, hxt-regex-xmlschema-9.2.0.7, hxt-unicode-9.0.2.4, hyphenation-0.8.3, indexed-traversable-0.1.4",
        "  - indexed-traversable-instances-0.1.2, integer-conversion-0.1.1, integer-logarithms-1.0.4, intervals-0.9.3, invariant-0.6.4, iproute-1.7.15, kan-extensions-5.2.7",
        "  ..." <> Pretty.endCode,
        "",
        Pretty.yellow <> "Building: 0" <> Pretty.endCode,
        "",
        Pretty.green <> "Completed: 0" <> Pretty.endCode
      ]

testFormatLargeBuilding1 :: TestTree
testFormatLargeBuilding1 = testFormatManual expected [ospPathSep|example_large_building_1.txt|]
  where
    expected =
      [ Pretty.magenta <> "To Build: 216",
        "  - BNFC-2.9.6.1, BNFC-meta-0.6.1, BiobaseNewick-0.0.0.2, Blammo-2.1.3.0, ChannelT-0.0.0.7, Chart-1.9.5, Chart-cairo-1.9.4.1, Chart-diagrams-1.9.5.1",
        "  - ChasingBottoms-1.3.1.16, ClustalParser-1.3.0, Color-0.4.0, DAV-1.3.4, DPutils-0.1.1.0, ENIG-0.0.1.0, Earley-0.13.0.1, FenwickTree-0.1.2.1, FontyFruity-0.5.3.5",
        "  - ForestStructures-0.0.1.1, GLFW-b-3.3.9.1, GLURaw-2.0.0.5, GLUT-2.7.0.16, Glob-0.10.2, HCodecs-0.5.2, HPDF-1.8, HSet-0.0.2, HStringTemplate-0.8.8, HTF-0.15.0.2",
        "  - HTTP-4000.4.1, HUnit-approx-1.1.1.1, HaTeX-3.23.0.1, HaXml-1.25.14, HandsomeSoup-0.4.2, HaskellNet-0.6.2, HaskellNet-SSL-0.4.0.2, Hclip-3.0.0.4, HsYAML-aeson-0.2.0.2",
        "  - IPv6Addr-2.0.6.1, IPv6DB-0.3.3.4, ImpSpec-0.1.0.0, Interpolation-0.3.0, JuicyCairo-0.1.0.1, JuicyPixels-3.3.9, JuicyPixels-extra-0.6.0, LPFP-1.1.5, ListLike-4.7.8.4",
        "  - ListZipper-1.2.0.2, MemoTrie-0.6.11, MissingH-1.6.0.2, MonadRandom-0.6.2, MultipletCombiner-0.0.7, MusicBrainz-0.4.1, NanoID-3.4.1.1, OpenAL-1.7.0.5, OpenGL-3.0.3.0",
        "  - Plural-0.0.2, QuasiText-0.1.2.6, QuickCheck-2.16.0.0, RSA-2.4.1, Ranged-sets-0.5.0, Rasterific-0.7.5.4, RefSerialize-0.4.0, SVGFonts-1.8.1, SciBaseTypes-0.1.1.0",
        "  - ShellCheck-0.11.0, Sit-0.2023.8.3, Vis-1.0.0, active-0.2.1, adjunctions-4.4.3, aeson-2.2.3.0, alex-meta-0.3.0.13, ansi-terminal-1.1.3, asn1-encoding-0.9.6",
        "  - asn1-parse-0.9.5, asn1-types-0.3.4, attoparsec-aeson-2.2.2.0, base64-1.0, bibtex-0.1.0.7, bifunctors-5.6.2, bindings-GLFW-3.3.9.2, bitvec-1.1.5.0, blaze-html-0.9.2.0",
        "  - blaze-svg-0.3.7, bytes-0.17.4, cairo-0.13.12.0, cairo-image-0.1.0.5, cborg-0.2.10.0, cereal-vector-0.2.0.1, comonad-5.0.9, conduit-1.3.6.1, conduit-extra-1.3.8",
        "  - cookie-0.5.1, cpphs-1.20.10, crypto-api-0.13.3, crypto-pubkey-types-0.4.3, crypton-1.0.4, crypton-connection-0.4.5, crypton-socks-0.6.2, crypton-x509-1.7.7",
        "  - crypton-x509-store-1.6.11, crypton-x509-system-1.6.7, crypton-x509-validation-1.6.14, diagrams-cairo-1.5, diagrams-core-1.5.1.1, diagrams-lib-1.5.0.1",
        "  - diagrams-postscript-1.5.3, diagrams-svg-1.5, distributive-0.6.2.1, dual-tree-0.2.3.1, ech-config-0.0.1, errors-2.3.0, fast-logger-3.2.6, free-5.2, generated-0.1.0.0",
        "  ..." <> Pretty.endCode,
        "",
        Pretty.yellow <> "Building: 7",
        "  - OpenGLRaw-3.3.4.1, async-2.2.5, case-insensitive-1.2.1.0, network-3.2.8.0, text-short-0.1.6, unordered-containers-0.2.20, zlib-0.7.1.1" <> Pretty.endCode,
        "",
        Pretty.green <> "Completed: 136",
        "  - Boolean-0.2.4, BoundedChan-1.0.3.0, Decimal-0.5.2, Diff-1.0.2, FailT-0.1.2.0, FindBin-0.0.5, FloatingHex-0.5, GenericPretty-1.2.2, HSlippyMap-3.0.1, HSvm-1.0.3.35",
        "  - HUnit-1.6.2.0, HasBigDecimal-0.2.0.0, HsYAML-0.2.1.5, IfElse-0.85, IntervalMap-0.6.2.1, LPFP-core-1.1.5, LetsBeRational-1.0.0.0, List-0.6.2, ListTree-0.2.3",
        "  - Mantissa-0.1.0.0, MonadPrompt-1.0.0.5, NoHoed-0.1.1, NumInstances-1.4, ObjectName-1.1.0.2, OneTuple-0.4.2, Only-0.1, PSQueue-1.2.2, ParsecTools-0.0.2.0, PyF-0.11.4.0",
        "  - RoundingFiasco-0.1.0.0, SHA-1.6.4.4, STMonadTrans-0.4.8.1, SafeSemaphore-0.10.1, StateVar-1.2.2, alex-3.5.4.0, ansi-terminal-types-1.1.3, appar-0.1.8, assoc-1.1.1",
        "  - attoparsec-0.14.4, auto-update-0.2.6, base-orphans-0.9.3, base16-bytestring-1.0.2.0, base64-bytestring-1.2.1.0, basement-0.0.16, binary-orphans-1.0.5",
        "  - bindings-DSL-1.0.25, blaze-builder-0.4.4.1, blaze-markup-0.8.3.0, bmp-1.2.6.4, byteorder-1.0.4, bytestring-lexing-0.5.0.15, c-enum-0.1.1.3, cabal-doctest-1.0.11",
        "  - call-stack-0.4.0, cereal-0.5.8.3, cereal-text-0.1.0.2, character-ps-0.1, clock-0.8.4, colour-2.3.6, context-0.2.1.1, contravariant-1.5.5, cryptohash-md5-0.11.101.0",
        "  - data-accessor-0.2.3.1, data-accessor-transformers-0.2.1.8, data-default-0.8.0.1, data-default-class-0.2.0.0, data-fix-0.3.4, data-lens-light-0.1.2.4",
        "  - diagrams-solve-0.1.3.1, dlist-1.0, easy-file-0.2.5, entropy-0.4.1.11, envparse-0.6.0, extra-1.8, fail-4.9.0.0, fgl-5.8.3.0, file-embed-0.0.16.0, fingertree-0.1.6.2",
        "  - fixed-0.3, fmlist-0.9.4, generically-0.1.1, groups-0.5.3, half-0.3.3, happy-lib-2.1.7, hashable-1.5.0.0, haskell-lexer-1.2.1, hourglass-0.2.12, hsc2hs-0.68.10",
        "  - hspec-discover-2.11.13, hxt-charproperties-9.5.0.0, indexed-traversable-0.1.4, integer-logarithms-1.0.4, logict-0.8.2.0, loop-0.3.0, mime-types-0.1.2.0",
        "  - monad-loops-0.4.3, network-byte-order-0.1.7, network-info-0.2.1, newtype-generics-0.6.2, old-locale-1.0.0.7, operational-0.2.4.2, parallel-3.2.2.0, polyparse-1.13.1",
        "  - prettyprinter-1.7.1, primitive-0.9.1.0, reflection-2.1.9, regex-base-0.94.0.3, safe-0.3.21, safe-exceptions-0.1.7.4, scanner-0.3.1, semigroups-0.20, split-0.2.5",
        "  ..." <> Pretty.endCode
      ]

testFormatLargeBuilding2 :: TestTree
testFormatLargeBuilding2 = testFormatManual expected [ospPathSep|example_large_building_2.txt|]
  where
    expected =
      [ Pretty.magenta <> "To Build: 104",
        "  - AesonBson-0.4.1, Agda-2.8.0, ListLike-4.7.8.4, abstract-deque-tests-0.3, acid-state-0.16.1.4, active-0.2.1, ad-4.5.6, adjunctions-4.4.3, advent-of-code-api-0.2.9.1",
        "  - aern2-mp-0.2.16.1, aern2-real-0.2.16.1, aeson-2.2.3.0, aeson-attoparsec-0.0.0, aeson-better-errors-0.9.1.3, aeson-casing-0.2.0.0, aeson-combinators-0.1.2.1",
        "  - aeson-diff-1.1.0.13, aeson-gadt-th-0.2.5.4, aeson-generic-compat-0.0.2.0, aeson-jsonpath-0.3.0.2, aeson-pretty-0.8.10, aeson-qq-0.8.4, aeson-schemas-1.4.3.0",
        "  - aeson-typescript-0.6.4.0, aeson-unqualified-ast-1.0.0.3, aeson-value-parser-0.19.7.2, aeson-warning-parser-0.1.1, aeson-yak-0.1.1.3, aeson-yaml-1.1.0.1",
        "  - alex-meta-0.3.0.13, alfred-margaret-2.1.0.2, algebra-4.3.1, alternative-vector-0.0.0, alternators-1.0.0.0, amqp-0.24.0, apecs-0.9.6, apecs-gloss-0.2.4",
        "  - apecs-physics-0.4.6, api-field-json-th-0.1.0.2, appendful-0.1.0.0, appendful-persistent-0.1.0.1, approximate-0.3.5, arithmoi-0.13.2.0, astro-0.4.3.0",
        "  - async-refresh-0.3.0.0, async-refresh-tokens-0.4.0.0, atom-conduit-0.9.0.2, attoparsec-aeson-2.2.2.0, autodocodec-0.5.0.0, bitvec-1.1.5.0, chimera-0.4.1.0",
        "  - conduit-1.3.6.1, conduit-extra-1.3.8, constraints-extras-0.4.0.2, crypton-connection-0.4.5, crypton-x509-system-1.6.7, crypton-x509-validation-1.6.14",
        "  - dependent-sum-0.7.2.0, edit-distance-vector-1.0.0.4, enummapset-0.7.3.0, generated-0.1.0.0, gloss-1.13.2.2, gloss-rendering-1.13.2.1, haskell-src-meta-0.8.15",
        "  - hspec-2.11.13, http-client-tls-0.3.6.4, indexed-traversable-instances-0.1.2, inline-c-0.9.1.10, kan-extensions-5.2.7, lens-5.3.5, libyaml-0.1.4, linear-1.23.2",
        "  - log-domain-0.13.2, matrix-0.3.6.4, mixed-types-num-0.6.2, mod-0.2.1.0, monad-logger-0.3.42, mono-traversable-1.0.21.0, monoid-subclasses-1.2.6",
        "  - nonempty-containers-0.3.5.0, nonempty-vector-0.2.4, persistent-2.17.1.0, pointed-5.0.5, process-extras-0.7.4, refined-0.8.2, rio-0.1.22.0, rio-prettyprint-0.1.8.0",
        "  - safecopy-0.10.4.3, semialign-1.3.1, serialise-0.2.6.1, servant-0.20.3.0, servant-client-0.20.3.0, servant-client-core-0.20.3.0, string-interpolate-0.3.4.0",
        "  ..." <> Pretty.endCode,
        "",
        Pretty.yellow <> "Building: 6",
        "  - GLUT-2.7.0.16, crypton-x509-store-1.6.11, free-5.2, haskell-src-exts-1.23.1, hspec-smallcheck-0.5.3, vector-0.13.2.0" <> Pretty.endCode,
        "",
        Pretty.green <> "Completed: 295",
        "  - AC-Angle-1.0, ANum-0.2.0.4, GLURaw-2.0.0.5, HUnit-1.6.2.0, ObjectName-1.1.0.2, OneTuple-0.4.2, OpenGL-3.0.3.0, OpenGLRaw-3.3.4.1, QuickCheck-2.16.0.0",
        "  - STMonadTrans-0.4.8.1, StateVar-1.2.2, Stream-0.4.7.2, abstract-deque-0.3, abstract-par-0.3.3, acc-0.2.0.3, ace-0.6, action-permutations-0.0.0.1, ad-delcont-0.5.0.0",
        "  - adler32-0.1.2.0, aftovolio-0.8.0.0, agreeing-0.2.2.0, alarmclock-0.7.0.7, alex-3.5.4.0, alex-tools-0.6.1, algebraic-graphs-0.7, almost-fix-0.0.2",
        "  - annotated-exception-0.3.0.4, annotated-wl-pprint-0.7.0, ansi-terminal-1.1.3, ansi-terminal-game-1.9.3.0, ansi-terminal-types-1.1.3, ansi-wl-pprint-1.0.2",
        "  - ap-normalize-0.1.0.1, appar-0.1.8, appendmap-0.1.5, apply-merge-0.1.1.0, apply-refact-0.15.0.0, apportionment-0.0.0.4, approximate-equality-1.1.0.2",
        "  - array-chunks-0.1.4.2, array-memoize-0.6.0, arrow-extras-0.1.0.1, arrows-0.4.4.2, ascii-char-1.0.1.0, ascii-progress-0.3.3.0, asn1-encoding-0.9.6, asn1-parse-0.9.5",
        "  - asn1-types-0.3.4, assert-failure-0.1.3.0, assignment-0.0.1.0, assoc-1.1.1, async-2.2.5, async-extra-0.2.0.0, async-pool-0.9.2, atom-basic-0.2.5, atomic-counter-0.1.2.3",
        "  - atomic-primops-0.8.8, atomic-write-0.2.1.1, attoparsec-0.14.4, attoparsec-base64-0.0.0, attoparsec-binary-0.2, attoparsec-data-1.0.5.4, attoparsec-expr-0.1.1.2",
        "  - attoparsec-time-1.0.3.1, auto-update-0.2.6, base-compat-0.14.1, base-orphans-0.9.3, base16-bytestring-1.0.2.0, base64-bytestring-1.2.1.0, basement-0.0.16",
        "  - bifunctors-5.6.2, binary-orphans-1.0.5, blaze-builder-0.4.4.1, blaze-html-0.9.2.0, blaze-markup-0.8.3.0, bmp-1.2.6.4, boring-0.2.2, boxes-0.1.5, bson-0.4.0.1",
        "  - byteorder-1.0.4, bytes-0.17.4, cabal-doctest-1.0.11, call-stack-0.4.0, case-insensitive-1.2.1.0, cborg-0.2.10.0, cdar-mBound-0.1.0.4, cereal-0.5.8.3, character-ps-0.1",
        "  - charset-0.3.12, cli-arguments-0.7.0.0, clock-0.8.4, collect-errors-0.1.6.0, colour-2.3.6, commutative-semigroups-0.2.0.2, comonad-5.0.9, concurrent-output-1.10.21",
        "  - constraints-0.14.2, contravariant-1.5.5, cookie-0.5.1, cryptohash-md5-0.11.101.0, cryptohash-sha1-0.11.101.0, crypton-1.0.4, crypton-socks-0.6.2, crypton-x509-1.7.7",
        "  ..." <> Pretty.endCode
      ]

testFormatManual :: [Text] -> OsPath -> TestTree
testFormatManual expected fileName = testCase desc $ do
  eResult <- runner $ do
    Monitor.readFormattedStatus
      Nothing
      Nothing
      path

  case eResult of
    Left err -> assertFailure $ "Received error: " ++ show err
    Right result -> do
      let resultLines = T.lines result
          numExpected = length expected
          numResult = length resultLines

          mkErr e r =
            mconcat
              [ "Expected\n  ",
                e,
                "\nReceived\n  ",
                r,
                "\nResult:\n",
                T.unpack result
              ]

      for_ (zip expected resultLines) $ \(e, r) -> do
        let msg = mkErr (T.unpack e) (T.unpack r)
        unless (e == r) $ assertFailure msg

      let msg = mkErr (show numExpected) (show numResult)

      assertBool msg (numExpected == numResult)
  where
    desc = "Formats file with inline and truncation: " ++ show fileName

    path = [ospPathSep|test/functional|] </> fileName

    runner =
      runEff
        . SState.evalState (Monitor.BuildWaiting, False)
        . PR.runPathReader
        . FR.runFileReader
        . runTerminalMock @Int (Just $ Window {height = 41, width = 174})

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
  IO TestArgs -> OsPath -> [String] -> IO (Set Text)
runMonitorLogs getTestArgs buildFileOsPath cliArgs = do
  testArgs <- getTestArgs
  logsRef <- newIORef mempty
  _ <-
    TO.timeout
      13_000_000
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
      -- I switched this from modifyIORef' as I thought this was causing a bug
      -- in the tests. That turned out to be false, but we should probably
      -- use this anyway as multiple threads are accessing this
      -- (timer thread and status printer).
      atomicModifyIORef' logsRef (\st -> (Set.insert txt st, ()))

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

unlineStrip :: [Text] -> Text
unlineStrip = T.strip . T.unlines

containsLog :: Text -> Set Text -> Bool
containsLog l logs =
  -- O(n) fallback for inexact match.
  (l `Set.member` logs) || any (T.isInfixOf l) logs
