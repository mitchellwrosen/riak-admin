module Riak.Admin.Command.BucketTypeSpec where

import Fixture
import Selectors

import Riak.Admin.Command.BucketType

import Prelude hiding (log)
import System.Exit
import Test.Hspec

spec :: Spec
spec =
  describe "bucketTypeList" $ do
    it "calls riak-admin bucket-type list" $ do
      let dict = ShellDict
            { _shellCode = \s -> do
                log s
                pure (ExitSuccess, "", "")
            }
      logShell bucketTypeList dict
        `shouldReturn` ["riak-admin bucket-type list"]

    it "throws an exception when exit code is non-zero" $ do
      let dict = ShellDict
            { _shellCode = \_ -> do
                pure (ExitFailure 1, "", "")
            }
      unShell bucketTypeList dict
        `shouldThrow` shellFailureCode 1

    it "parses active bucket type" $ do
      let dict = ShellDict
            { _shellCode = \_ -> do
                pure (ExitSuccess, "default (active)", "")
            }
      unShell bucketTypeList dict
        `shouldReturn` [("default", True)]

    it "parses inactive bucket type" $ do
      let dict = ShellDict
            { _shellCode = \_ -> do
                pure (ExitSuccess, "default (not active)", "")
            }
      unShell bucketTypeList dict
        `shouldReturn` [("default", False)]

    it "parses a list of bucket types" $ do
      let dict = ShellDict
            { _shellCode = \_ -> do
                pure (ExitSuccess, "default (active)\nfoobar (not active)", "")
            }
      unShell bucketTypeList dict
        `shouldReturn` [("default", True), ("foobar", False)]
