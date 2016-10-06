module Riak.Admin.Command.BucketTypeSpec where

import Fixture
import Selectors

import Riak.Admin.Command.BucketType
import Riak.Admin.Types
import Riak.Admin.Types.BucketProps

import Data.Default.Class
import Data.Text          (Text)
import Prelude            hiding (log)
import System.Exit
import Test.Hspec

spec :: Spec
spec = do
  describe "bucketTypeCreate" createSpec
  describe "bucketTypeList"   listSpec

createSpec :: Spec
createSpec = do
  it "calls riak-admin bucket-type create" $
    logShell (bucketTypeCreate "foo" def) logDict
      `shouldReturn` ["riak-admin bucket-type create foo '{\"props\":{}}'"]

  it "throws an exception when exit code is non-zero" $ do
    let dict = ShellDict { _shellCode = \_ -> pure (ExitFailure 1, "", "") }
    unShell (bucketTypeCreate "foo" def) dict
      `shouldThrow` shellFailureCode 1

  it "sets allow_mult" $
    logShell (bucketTypeCreate "foo" (def { allow_mult = Just True })) logDict
      `shouldReturn` ["riak-admin bucket-type create foo '{\"props\":{\"allow_mult\":true}}'"]

  it "sets allow_mult" $
    logShell (bucketTypeCreate "foo" (def { allow_mult = Just True })) logDict
      `shouldReturn` ["riak-admin bucket-type create foo '{\"props\":{\"allow_mult\":true}}'"]

  it "sets basic_quorum" $
    logShell (bucketTypeCreate "foo" (def { basic_quorum = Just True })) logDict
      `shouldReturn` ["riak-admin bucket-type create foo '{\"props\":{\"basic_quorum\":true}}'"]

  it "sets big_vclock" $
    logShell (bucketTypeCreate "foo" (def { big_vclock = Just 10 })) logDict
      `shouldReturn` ["riak-admin bucket-type create foo '{\"props\":{\"big_vclock\":10}}'"]

  it "sets datatype" $
    logShell (bucketTypeCreate "foo" (def { datatype = Just CounterType })) logDict
      `shouldReturn` ["riak-admin bucket-type create foo '{\"props\":{\"datatype\":\"counter\"}}'"]

  it "sets dvv_enabled" $
    logShell (bucketTypeCreate "foo" (def { dvv_enabled = Just True })) logDict
      `shouldReturn` ["riak-admin bucket-type create foo '{\"props\":{\"dvv_enabled\":true}}'"]

  it "sets dw" $
    logShell (bucketTypeCreate "foo" (def { dw = Just All })) logDict
      `shouldReturn` ["riak-admin bucket-type create foo '{\"props\":{\"dw\":\"all\"}}'"]

  it "sets last_write_wins" $
    logShell (bucketTypeCreate "foo" (def { last_write_wins = Just False })) logDict
      `shouldReturn` ["riak-admin bucket-type create foo '{\"props\":{\"last_write_wins\":false}}'"]

  it "sets n_val" $
    logShell (bucketTypeCreate "foo" (def { n_val = Just 5 })) logDict
      `shouldReturn` ["riak-admin bucket-type create foo '{\"props\":{\"n_val\":5}}'"]

  it "sets notfound_ok" $
    logShell (bucketTypeCreate "foo" (def { notfound_ok = Just False })) logDict
      `shouldReturn` ["riak-admin bucket-type create foo '{\"props\":{\"notfound_ok\":false}}'"]

  it "sets old_vclock" $
    logShell (bucketTypeCreate "foo" (def { old_vclock = Just 10 })) logDict
      `shouldReturn` ["riak-admin bucket-type create foo '{\"props\":{\"old_vclock\":10}}'"]

  it "sets postcommit" $
    logShell (bucketTypeCreate "foo" (def { postcommit = Just ["bar", "baz"] })) logDict
      `shouldReturn` ["riak-admin bucket-type create foo '{\"props\":{\"postcommit\":[\"bar\",\"baz\"]}}'"]

  it "sets pr" $
    logShell (bucketTypeCreate "foo" (def { pr = Just Default })) logDict
      `shouldReturn` ["riak-admin bucket-type create foo '{\"props\":{\"pr\":\"default\"}}'"]

  it "sets precommit" $
    logShell (bucketTypeCreate "foo" (def { precommit = Just ["bar", "baz"] })) logDict
      `shouldReturn` ["riak-admin bucket-type create foo '{\"props\":{\"precommit\":[\"bar\",\"baz\"]}}'"]

  it "sets pw" $
    logShell (bucketTypeCreate "foo" (def { pw = Just One })) logDict
      `shouldReturn` ["riak-admin bucket-type create foo '{\"props\":{\"pw\":\"one\"}}'"]

  it "sets r" $
    logShell (bucketTypeCreate "foo" (def { r = Just Quorum })) logDict
      `shouldReturn` ["riak-admin bucket-type create foo '{\"props\":{\"r\":\"quorum\"}}'"]

  it "sets rw" $
    logShell (bucketTypeCreate "foo" (def { rw = Just (N 10) })) logDict
      `shouldReturn` ["riak-admin bucket-type create foo '{\"props\":{\"rw\":10}}'"]

  it "sets small_vclock" $
    logShell (bucketTypeCreate "foo" (def { small_vclock = Just 5 })) logDict
      `shouldReturn` ["riak-admin bucket-type create foo '{\"props\":{\"small_vclock\":5}}'"]

  it "sets w" $
    logShell (bucketTypeCreate "foo" (def { w = Just (N 1) })) logDict
      `shouldReturn` ["riak-admin bucket-type create foo '{\"props\":{\"w\":1}}'"]

  it "sets young_vclock" $
    logShell (bucketTypeCreate "foo" (def { young_vclock = Just 5 })) logDict
      `shouldReturn` ["riak-admin bucket-type create foo '{\"props\":{\"young_vclock\":5}}'"]

  it "sets multiple properties" $
    logShell (bucketTypeCreate "foo" (def { r = Just All, w = Just One })) logDict
      `shouldReturn` ["riak-admin bucket-type create foo '{\"props\":{\"r\":\"all\",\"w\":\"one\"}}'"]


-- | A ShellDict that just logs its argument and succeeds.
logDict :: ShellDict s
logDict = ShellDict { _shellCode = \s -> log s >> succeed }

listSpec :: Spec
listSpec = do
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

succeed :: Applicative m => m (ExitCode, Text, Text)
succeed = pure (ExitSuccess, "", "")
