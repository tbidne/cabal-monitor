{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import Cabal.Monitor.BuildStatus
  ( BuildStatus
      ( MkBuildStatus,
        building,
        buildingLocal,
        completed,
        toBuild
      ),
    BuildStatusInit,
    Package (MkPackage),
  )
import Cabal.Monitor.BuildStatus qualified as BuildStatus
import Cabal.Monitor.Config
  ( Coloring (unColoring),
    Config
      ( coloring,
        filePath,
        height,
        localPackages,
        period,
        pid,
        searchInfix,
        width
      ),
    LocalPackages (unLocalPackages),
    SearchInfix (unSearchInfix),
  )
import Cabal.Monitor.Config qualified as Config
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as C8
import Data.List qualified as L
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Effectful (Eff, IOE, runEff, type (:>))
import Effectful.Dispatch.Dynamic (passthrough, reinterpret)
import Effectful.FileSystem.FileReader.Static qualified as FR
import Effectful.FileSystem.PathReader.Dynamic (PathReader)
import Effectful.FileSystem.PathReader.Dynamic qualified as PR
import Effectful.Optparse.Static qualified as Optparse
import FileSystem.OsPath (OsPath, decodeLenient, osp, ospPathSep)
import FileSystem.UTF8 qualified as UTF8
import Hedgehog
  ( Gen,
    PropertyName,
    PropertyT,
    annotateShow,
    assert,
    forAll,
    property,
    (===),
  )
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Environment qualified as Env
import Test.Tasty
  ( TestName,
    TestTree,
    defaultMain,
    testGroup,
  )
import Test.Tasty.HUnit (testCase, (@=?))
import Test.Tasty.Hedgehog (testPropertyNamed)

main :: IO ()
main =
  defaultMain $
    testGroup
      "Unit"
      [ configTests,
        testAdvancePhase,
        lineSplitTest
      ]

configTests :: TestTree
configTests =
  testGroup
    "Config"
    [ testDefaultConfig,
      testDirectToml,
      testXdgToml,
      testArgsOverridesToml
    ]

testDefaultConfig :: TestTree
testDefaultConfig = testCase desc $ do
  config <- runConfig args Nothing

  True @=? config.coloring.unColoring
  [ospPathSep|file.log|] @=? config.filePath
  Nothing @=? config.height
  True @=? config.localPackages.unLocalPackages
  5 @=? config.period
  Nothing @=? config.pid
  False @=? config.searchInfix.unSearchInfix
  Nothing @=? config.width
  where
    desc = "Default config"
    args =
      [ "-f",
        "file.log"
      ]

testDirectToml :: TestTree
testDirectToml = testCase desc $ do
  config <- runConfig args Nothing

  False @=? config.coloring.unColoring
  [ospPathSep|file.log|] @=? config.filePath
  Just 15 @=? config.height
  False @=? config.localPackages.unLocalPackages
  20 @=? config.period
  Nothing @=? config.pid
  True @=? config.searchInfix.unSearchInfix
  Just 20 @=? config.width
  where
    desc = "Uses direct toml config"
    args =
      [ "-f",
        "file.log",
        "-c",
        decodeLenient tomlPath
      ]

    tomlPath = [ospPathSep|test/unit/config.toml|]

testXdgToml :: TestTree
testXdgToml = testCase desc $ do
  config <- runConfig args (Just tomlPath)

  False @=? config.coloring.unColoring
  [ospPathSep|file.log|] @=? config.filePath
  Just 15 @=? config.height
  False @=? config.localPackages.unLocalPackages
  20 @=? config.period
  Nothing @=? config.pid
  True @=? config.searchInfix.unSearchInfix
  Just 20 @=? config.width
  where
    desc = "Uses xdg toml config"
    args =
      [ "-f",
        "file.log"
      ]

    tomlPath = [ospPathSep|test/unit/config.toml|]

testArgsOverridesToml :: TestTree
testArgsOverridesToml = testCase desc $ do
  config <- runConfig args Nothing

  True @=? config.coloring.unColoring
  [ospPathSep|file.log|] @=? config.filePath
  Just 7 @=? config.height
  True @=? config.localPackages.unLocalPackages
  2 @=? config.period
  Just 123 @=? config.pid
  False @=? config.searchInfix.unSearchInfix
  Just 9 @=? config.width
  where
    desc = "Args overrides toml"
    args =
      [ "-f",
        "file.log",
        "-c",
        decodeLenient tomlPath,
        "--color",
        "on",
        "--height",
        "7",
        "--local-packages",
        "on",
        "--period",
        "2",
        -- pid is technically not a toml override, but we test it here for
        -- convenience.
        "--pid",
        "123",
        "--search-infix",
        "off",
        "--width",
        "9"
      ]

    tomlPath = [ospPathSep|test/unit/config.toml|]

runConfig :: [String] -> Maybe OsPath -> IO Config
runConfig args mXdgPath = Env.withArgs args $ runner Config.getConfig
  where
    runner =
      runEff
        . FR.runFileReader
        . Optparse.runOptparse
        . runPathReader

    runPathReader :: (IOE :> es) => Eff (PathReader : es) a -> Eff es a
    runPathReader = reinterpret PR.runPathReader $ \env -> \case
      PR.DoesFileExist p -> do
        let pStr = decodeLenient p
        if "xdg" `L.isInfixOf` pStr
          then pure False
          else PR.doesFileExist p
      PR.GetXdgDirectory _ _ -> pure $ fromMaybe [osp|xdg|] mXdgPath
      other -> passthrough env other

testAdvancePhase :: TestTree
testAdvancePhase = testProp desc "testAdvancePhase" $ do
  statusInit <- forAll genBuildStatus

  validBuildStatus statusInit

  let statusFinal = BuildStatus.advancePhase statusInit

  length statusInit.toBuild === BuildStatus.numAllPkgs statusFinal

  assert $ statusFinal.toBuild `Set.disjoint` statusFinal.building
  assert $ statusFinal.toBuild `Set.disjoint` statusFinal.completed
  assert $ statusFinal.building `Set.disjoint` statusFinal.completed
  mempty === statusFinal.buildingLocal
  where
    desc = "BuildStatus advancePhase invariants"

validBuildStatus :: BuildStatusInit -> PropertyT IO ()
validBuildStatus status = do
  annotateShow status
  assert $ building `Set.isSubsetOf` toBuild
  assert $ status.buildingLocal `Set.isSubsetOf` toBuild
  assert $ completed `Set.isSubsetOf` allBuilding
  assert $ building `Set.disjoint` buildingLocal
  where
    toBuild = status.toBuild
    building = status.building
    buildingLocal = status.buildingLocal
    completed = status.completed

    allBuilding = building `Set.union` buildingLocal

genBuildStatus :: Gen BuildStatusInit
genBuildStatus = do
  toBuild <- genPackageSet
  allBuilding <- Gen.subset toBuild
  building <- Gen.subset allBuilding
  let buildingLocal = allBuilding `Set.difference` building
  completed <- Gen.subset allBuilding
  pure $
    MkBuildStatus
      { toBuild,
        building,
        buildingLocal,
        completed
      }

testProp :: TestName -> PropertyName -> PropertyT IO () -> TestTree
testProp desc propName = testPropertyNamed desc propName . property

genPackageSet :: Gen (Set Package)
genPackageSet = Set.fromList <$> Gen.list (Range.linearFrom 0 0 50) genPackage

genPackage :: Gen Package
genPackage =
  MkPackage
    . UTF8.encodeUtf8
    <$> Gen.text (Range.linearFrom 0 0 10) Gen.lower

lineSplitTest :: TestTree
lineSplitTest =
  testGroup
    "lines"
    [ testLinesNlEq,
      testLinesCrNlEq,
      testLinesCrLfCases
    ]

testLinesNlEq :: TestTree
testLinesNlEq = testProp desc "testLinesNlEq" $ do
  bs <- forAll genBsNoCr
  let r = BuildStatus.linesCrLf bs
      e = C8.lines bs

  r === e
  where
    desc = "linesCrLf lf == C8.lines lf"

testLinesCrNlEq :: TestTree
testLinesCrNlEq = testProp desc "testLinesCrNlEq" $ do
  (nl, crnl) <- forAll genNlAndCrlf
  let r = BuildStatus.linesCrLf crnl
      e = C8.lines nl

  r === e
  where
    desc = "linesCrLf crlf == C8.lines nl"

testLinesCrLfCases :: TestTree
testLinesCrLfCases = testCase desc $ do
  [] @=? C8.lines ""
  [] @=? BuildStatus.linesCrLf ""

  [""] @=? C8.lines "\n"
  [""] @=? BuildStatus.linesCrLf "\n"
  [""] @=? BuildStatus.linesCrLf "\r\n"
  ["\r"] @=? BuildStatus.linesCrLf "\r"

  ["", ""] @=? C8.lines "\n\n"
  ["", ""] @=? BuildStatus.linesCrLf "\n\n"
  ["", ""] @=? BuildStatus.linesCrLf "\r\n\n"
  ["", ""] @=? BuildStatus.linesCrLf "\n\r\n"
  ["", ""] @=? BuildStatus.linesCrLf "\r\n\r\n"

  ["", "ab", "cd"] @=? C8.lines "\nab\ncd\n"
  ["", "ab", "cd"] @=? BuildStatus.linesCrLf "\nab\ncd\n"
  ["", "ab", "cd"] @=? BuildStatus.linesCrLf "\r\nab\ncd\n"
  ["", "ab", "cd"] @=? BuildStatus.linesCrLf "\nab\r\ncd\n"
  ["", "ab", "cd"] @=? BuildStatus.linesCrLf "\nab\ncd\r\n"
  ["", "ab", "cd"] @=? BuildStatus.linesCrLf "\r\nab\r\ncd\n"
  ["", "ab", "cd"] @=? BuildStatus.linesCrLf "\nab\r\ncd\r\n"
  ["", "ab", "cd"] @=? BuildStatus.linesCrLf "\nab\r\ncd\r\n"
  ["", "ab", "cd"] @=? BuildStatus.linesCrLf "\r\nab\r\ncd\r\n"
  where
    desc = "linesCrLf cases"

genBsNoCr :: Gen ByteString
genBsNoCr = UTF8.encodeUtf8 . T.replace "\r" "" <$> genTextNoCr

-- | Generates bytestrings where the first may have newlines, and the second
-- has all newlines replaced w/ crlf.
genNlAndCrlf :: Gen (ByteString, ByteString)
genNlAndCrlf = f <$> genTextNoCr
  where
    f t =
      ( UTF8.encodeUtf8 t,
        UTF8.encodeUtf8 . T.replace "\n" "\r\n" $ t
      )

genTextNoCr :: Gen Text
genTextNoCr =
  fmap (T.replace "\r" "")
    . Gen.text (Range.linearFrom 0 0 10)
    $ Gen.unicode
