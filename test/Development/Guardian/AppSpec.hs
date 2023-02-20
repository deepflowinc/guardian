{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}

module Development.Guardian.AppSpec (test_defaultMainWith) where

import Control.Exception
import Control.Monad (void)
import qualified Data.ByteString.Builder as BB
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Data.Version (showVersion)
import Development.Guardian.App
import Development.Guardian.Flags (cabalEnabled, stackEnabled)
import Development.Guardian.Graph.Adapter.Detection
import Development.Guardian.Test.Flags
import GHC.IO.Exception (ExitCode (..))
import Path
import Path.IO
import Paths_guardian (version)
import RIO (logOptionsMemory, mkSimpleApp, readIORef, runRIO, runSimpleApp, withLogFunc)
import qualified RIO.ByteString.Lazy as LBS
import RIO.Process (proc, runProcess_)
import Test.Tasty
import Test.Tasty.ExpectedFailure (ignoreTestBecause)
import Test.Tasty.HUnit

fakeBuildInfo :: BuildInfo
fakeBuildInfo = BuildInfo {versionString = showVersion version, gitInfo = Nothing}

test_defaultMainWith :: TestTree
test_defaultMainWith =
  testGroup "defaultMainWith" $
    concreteAdapterTests : stackCases ++ cabalCases ++ autoCases ++ customCases

customCases :: [TestTree]
customCases =
  [ testGroup
      "custom adapter"
      [ testConditional "cabal-plan" testCabalPlan
          $ testCase "works with cabal-plan dot"
          $ withCurrentDir
            ([reldir|data|] </> [reldir|only-custom-cabal-plan|])
          $ do
            runSimpleApp $ proc "cabal" ["v2-update"] runProcess_
            runSimpleApp $ proc "cabal" ["v2-build", "--dry-run", "all"] runProcess_
            successfully_ $
              mainWith ["custom"]
      , testConditional "graphmod" testGraphmod $
          testCase "works with graphmod" $
            successfully_ $
              mainWith ["custom", "-c", "dependency-domains-graphmod.yaml"]
      , testCase "works with stack dot"
          $ withCurrentDir
            ([reldir|data|] </> [reldir|only-stack|])
          $ successfully_
          $ mainWith ["custom", "-c", "dependency-domains-stack-dot.yaml"]
      ]
  ]

stackCases :: [TestTree]
stackCases =
  [ skipIfStackDisabled $
      testGroup
        "stack-specific options"
        [ testCase "Respects --stack-yaml"
            $ withCurrentDir
              ([reldir|data|] </> [reldir|test-only-dependency|])
            $ successfully_
            $ mainWith ["cabal", "-c", "dependency-domains-custom-stack.yaml"]
        ]
  ]

cabalCases :: [TestTree]
cabalCases =
  [ skipIfCabalDisabled $
      testGroup
        "cabal-specific options"
        [ testCase "Respects projectFile"
            $ withCurrentDir
              ([reldir|data|] </> [reldir|test-only-dependency|])
            $ successfully_
            $ mainWith ["cabal", "-c", "dependency-domains-custom-cabal.yaml"]
        , testCase "Respects update: true"
            $ withCurrentDir
              ([reldir|data|] </> [reldir|test-only-dependency|])
            $ successfully_
            $ mainWith ["cabal", "-c", "dependency-domains-cabal-update-true.yaml"]
        , testCase "Respects update: (index-state)"
            $ withCurrentDir
              ([reldir|data|] </> [reldir|test-only-dependency|])
            $ successfully_
            $ mainWith ["cabal", "-c", "dependency-domains-cabal-update-index.yaml"]
        ]
  ]

skipIfCabalDisabled :: TestTree -> TestTree
skipIfCabalDisabled = testConditional "cabal" cabalEnabled

skipIfStackDisabled :: TestTree -> TestTree
skipIfStackDisabled = testConditional "stack" stackEnabled

testConditional :: String -> Bool -> TestTree -> TestTree
testConditional label testIt =
  if testIt
    then id
    else ignoreTestBecause $ "Test disabled for " <> label

autoCases :: [TestTree]
autoCases =
  [ testGroup
      "Auto detection"
      [ skipIfCabalDisabled
          $ testCase "Accepts config with cabal section only"
          $ withCurrentDir
            ([reldir|data|] </> [reldir|test-only-dependency|])
          $ successfully
            (LT.isInfixOf "with backend Cabal" . LT.decodeUtf8)
          $ mainWith ["auto", "-c", "dependency-domains-custom-cabal.yaml"]
      , skipIfStackDisabled
          $ testCase "Accepts config with stack section only"
          $ withCurrentDir
            ([reldir|data|] </> [reldir|test-only-dependency|])
          $ successfully
            (LT.isInfixOf "with backend Stack" . LT.decodeUtf8)
          $ mainWith ["auto", "-c", "dependency-domains-custom-stack.yaml"]
      , skipIfCabalDisabled
          $ testCase "Accepts unambiguous directory (cabal)"
          $ withCurrentDir
            ([reldir|data|] </> [reldir|only-cabal|])
          $ successfully
            (LT.isInfixOf "with backend Cabal" . LT.decodeUtf8)
          $ mainWith ["auto"]
      , skipIfStackDisabled
          $ testCase "Accepts unambiguous directory (stack)"
          $ withCurrentDir
            ([reldir|data|] </> [reldir|only-stack|])
          $ successfully
            (LT.isInfixOf "with backend Stack" . LT.decodeUtf8)
          $ mainWith ["auto"]
      , testCaseSteps "Rejects ambiguous inputs" \step ->
          withCurrentDir
            ([reldir|data|] </> [reldir|test-only-dependency|])
            $ do
              step "Abmiguous Directory & Config without any custom section"
              mainWith ["auto"] `shouldThrow` (== NoCustomConfigSpecified)
              step "Abmiguous Directory & Config wit both custom sections"
              mainWith ["auto", "-c", "dependency-domains-ambiguous.yaml"]
                `shouldThrow` (== MultipleAdapterConfigFound)
      ]
  ]

concreteAdapterTests :: TestTree
concreteAdapterTests =
  testGroup
    "Concrete adapter behaviours, independent of adapters"
    [ skipIfDisabled $
      testGroup
        backend
        [ testCase "invalidates test-only-dependency with default config"
            $ withCurrentDir
              ([reldir|data|] </> [reldir|test-only-dependency|])
            $ mainWith [backend] `shouldThrow` (== ExitFailure 1)
        , testCaseSteps "invalidates test-only-dependency with default config (explicit path argument)" \step -> do
            step "Absolute dir"
            dir <- canonicalizePath ([reldir|data|] </> [reldir|test-only-dependency|])
            mainWith [backend, fromAbsDir dir] `shouldThrow` (== ExitFailure 1)
            step "Relative dir"
            let rdir = [reldir|data|] </> [reldir|test-only-dependency|]
            mainWith [backend, fromRelDir rdir] `shouldThrow` (== ExitFailure 1)
        , testCaseSteps "accepts non-standard config yaml" $ \step ->
            withCurrentDir ([reldir|data|] </> [reldir|test-only-dependency|]) $ do
              step "Accepts  when tests and benchmarks disabled"
              successfully_ $
                mainWith [backend, "-c", "dependency-domains-no-tests-benchmarks.yaml"]
              step "Accepts input with exception rule"
              successfully (LT.isInfixOf "exceptional rules are used" . LT.decodeUtf8) $
                mainWith [backend, "-c", "dependency-domains-except-A2-B1.yaml"]
        ]
    | (backend, skipIfDisabled) <-
        [ ("cabal", skipIfCabalDisabled)
        , ("stack", skipIfStackDisabled)
        ]
    ]

successfully_ :: HasCallStack => IO (LBS.ByteString, Maybe SomeException) -> IO ()
successfully_ = void . successfully (const True)

successfully :: HasCallStack => (LBS.ByteString -> Bool) -> IO (LBS.ByteString, Maybe SomeException) -> IO ()
successfully isOk act = do
  (a, exc) <- act
  case exc of
    Just err ->
      assertFailure $
        "Exception: "
          <> displayException err
          <> "\nwith log: \n"
          <> LT.unpack (LT.decodeUtf8 a)
    Nothing
      | isOk a -> pure ()
      | otherwise ->
          assertFailure $
            "Exits Successfully, but the output is invalid: \n"
              <> LT.unpack (LT.decodeUtf8 a)

shouldThrow :: (HasCallStack, Exception e) => IO (a, Maybe e) -> (e -> Bool) -> Assertion
shouldThrow act p =
  act >>= \case
    (_, Nothing) ->
      assertFailure
        "Expected to throw excetpion, but exists successfully"
    (_, Just err)
      | p err -> pure ()
      | otherwise ->
          assertFailure $
            "Exception has been thrown, but does not satisfy the requirement: "
              <> show err

mainWith :: Exception exc => [String] -> IO (LBS.ByteString, Maybe exc)
mainWith args = do
  (logs, opts) <- logOptionsMemory
  eith <- try $ withLogFunc opts $ \logFunc -> do
    app <- mkSimpleApp logFunc Nothing
    runRIO app $
      defaultMainWith fakeBuildInfo args
  (,either Just (const Nothing) eith) . BB.toLazyByteString
    <$> readIORef logs
