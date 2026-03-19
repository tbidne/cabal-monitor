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
import Data.Set (Set)
import Data.Set qualified as Set
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
import Test.Tasty
  ( TestName,
    TestTree,
    defaultMain,
    testGroup,
  )
import Test.Tasty.Hedgehog (testPropertyNamed)

main :: IO ()
main =
  defaultMain $
    testGroup
      "Unit"
      [ testAdvancePhase
      ]

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
