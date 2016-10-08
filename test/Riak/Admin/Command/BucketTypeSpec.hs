module Riak.Admin.Command.BucketTypeSpec where

import Fixture
import Selectors

import Riak.Admin.Internal

import Control.Exception
import Control.Monad
import Data.Default.Class
import Data.Text          (Text)
import Data.Typeable      (typeOf)
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
      `shouldBeRight` ["riak-admin bucket-type create foo '{\"props\":{}}'"]

  it "throws an exception when exit code is non-zero" $ do
    let dict = ShellDict { _shellCode = \_ -> pure (ExitFailure 1, "", "") }
    unShell (bucketTypeCreate "foo" def) dict
      `shouldThrowLeft` shellFailureCode 1

  it "sets allow_mult" $
    logShell (bucketTypeCreate "foo" (def { allow_mult = Just True })) logDict
      `shouldBeRight` ["riak-admin bucket-type create foo '{\"props\":{\"allow_mult\":true}}'"]

  it "sets allow_mult" $
    logShell (bucketTypeCreate "foo" (def { allow_mult = Just True })) logDict
      `shouldBeRight` ["riak-admin bucket-type create foo '{\"props\":{\"allow_mult\":true}}'"]

  it "sets basic_quorum" $
    logShell (bucketTypeCreate "foo" (def { basic_quorum = Just True })) logDict
      `shouldBeRight` ["riak-admin bucket-type create foo '{\"props\":{\"basic_quorum\":true}}'"]

  it "sets big_vclock" $
    logShell (bucketTypeCreate "foo" (def { big_vclock = Just 10 })) logDict
      `shouldBeRight` ["riak-admin bucket-type create foo '{\"props\":{\"big_vclock\":10}}'"]

  it "sets datatype" $
    logShell (bucketTypeCreate "foo" (def { datatype = Just CounterType })) logDict
      `shouldBeRight` ["riak-admin bucket-type create foo '{\"props\":{\"datatype\":\"counter\"}}'"]

  it "sets dvv_enabled" $
    logShell (bucketTypeCreate "foo" (def { dvv_enabled = Just True })) logDict
      `shouldBeRight` ["riak-admin bucket-type create foo '{\"props\":{\"dvv_enabled\":true}}'"]

  it "sets dw" $
    logShell (bucketTypeCreate "foo" (def { dw = Just All })) logDict
      `shouldBeRight` ["riak-admin bucket-type create foo '{\"props\":{\"dw\":\"all\"}}'"]

  it "sets last_write_wins" $
    logShell (bucketTypeCreate "foo" (def { last_write_wins = Just False })) logDict
      `shouldBeRight` ["riak-admin bucket-type create foo '{\"props\":{\"last_write_wins\":false}}'"]

  it "sets n_val" $
    logShell (bucketTypeCreate "foo" (def { n_val = Just 5 })) logDict
      `shouldBeRight` ["riak-admin bucket-type create foo '{\"props\":{\"n_val\":5}}'"]

  it "sets notfound_ok" $
    logShell (bucketTypeCreate "foo" (def { notfound_ok = Just False })) logDict
      `shouldBeRight` ["riak-admin bucket-type create foo '{\"props\":{\"notfound_ok\":false}}'"]

  it "sets old_vclock" $
    logShell (bucketTypeCreate "foo" (def { old_vclock = Just 10 })) logDict
      `shouldBeRight` ["riak-admin bucket-type create foo '{\"props\":{\"old_vclock\":10}}'"]

  it "sets postcommit" $
    logShell (bucketTypeCreate "foo" (def { postcommit = Just ["bar", "baz"] })) logDict
      `shouldBeRight` ["riak-admin bucket-type create foo '{\"props\":{\"postcommit\":[\"bar\",\"baz\"]}}'"]

  it "sets pr" $
    logShell (bucketTypeCreate "foo" (def { pr = Just Default })) logDict
      `shouldBeRight` ["riak-admin bucket-type create foo '{\"props\":{\"pr\":\"default\"}}'"]

  it "sets precommit" $
    logShell (bucketTypeCreate "foo" (def { precommit = Just ["bar", "baz"] })) logDict
      `shouldBeRight` ["riak-admin bucket-type create foo '{\"props\":{\"precommit\":[\"bar\",\"baz\"]}}'"]

  it "sets pw" $
    logShell (bucketTypeCreate "foo" (def { pw = Just One })) logDict
      `shouldBeRight` ["riak-admin bucket-type create foo '{\"props\":{\"pw\":\"one\"}}'"]

  it "sets r" $
    logShell (bucketTypeCreate "foo" (def { r = Just Quorum })) logDict
      `shouldBeRight` ["riak-admin bucket-type create foo '{\"props\":{\"r\":\"quorum\"}}'"]

  it "sets rw" $
    logShell (bucketTypeCreate "foo" (def { rw = Just (N 10) })) logDict
      `shouldBeRight` ["riak-admin bucket-type create foo '{\"props\":{\"rw\":10}}'"]

  it "sets small_vclock" $
    logShell (bucketTypeCreate "foo" (def { small_vclock = Just 5 })) logDict
      `shouldBeRight` ["riak-admin bucket-type create foo '{\"props\":{\"small_vclock\":5}}'"]

  it "sets w" $
    logShell (bucketTypeCreate "foo" (def { w = Just (N 1) })) logDict
      `shouldBeRight` ["riak-admin bucket-type create foo '{\"props\":{\"w\":1}}'"]

  it "sets young_vclock" $
    logShell (bucketTypeCreate "foo" (def { young_vclock = Just 5 })) logDict
      `shouldBeRight` ["riak-admin bucket-type create foo '{\"props\":{\"young_vclock\":5}}'"]

  it "sets multiple properties" $
    logShell (bucketTypeCreate "foo" (def { r = Just All, w = Just One })) logDict
      `shouldBeRight` ["riak-admin bucket-type create foo '{\"props\":{\"r\":\"all\",\"w\":\"one\"}}'"]

listSpec :: Spec
listSpec = do
  it "calls riak-admin bucket-type list" $ do
    let dict = ShellDict
          { _shellCode = \s -> do
              log s
              pure (ExitSuccess, "", "")
          }
    logShell bucketTypeList dict
      `shouldBeRight` ["riak-admin bucket-type list"]

  it "throws an exception when exit code is non-zero" $ do
    let dict = ShellDict
          { _shellCode = \_ -> do
              pure (ExitFailure 1, "", "")
          }
    unShell bucketTypeList dict
      `shouldThrowLeft` shellFailureCode 1

  it "parses active bucket type" $ do
    let dict = ShellDict
          { _shellCode = \_ -> do
              pure (ExitSuccess, "default (active)", "")
          }
    unShell bucketTypeList dict
      `shouldBeRight` [("default", Active)]

  it "parses inactive bucket type" $ do
    let dict = ShellDict
          { _shellCode = \_ -> do
              pure (ExitSuccess, "default (not active)", "")
          }
    unShell bucketTypeList dict
      `shouldBeRight` [("default", NotActive)]

  it "parses a list of bucket types" $ do
    let dict = ShellDict
          { _shellCode = \_ -> do
              pure (ExitSuccess, "default (active)\nfoobar (not active)", "")
          }
    unShell bucketTypeList dict
      `shouldBeRight` [("default", Active), ("foobar", NotActive)]

  it "parses unicode bucket types" $ do
    let dict = ShellDict
          { _shellCode = \_ -> do
              pure (ExitSuccess, "❤✓☀★ (active)", "")
          }
    unShell bucketTypeList dict
      `shouldBeRight` [("❤✓☀★", Active)]

shouldBeRight
  :: (Eq a, Show a) => Either SomeException a -> a -> Expectation
shouldBeRight (Left e) _ = expectationFailure (displayException e)
shouldBeRight (Right a) b = a `shouldBe` b

shouldThrowLeft
  :: forall a e.
     (Show a, Exception e)
  => Either SomeException a -> Selector e -> Expectation
shouldThrowLeft e p =
  case e of
    Left e' ->
      case fromException e' of
        Nothing -> throwIO e'
        Just e'' ->
          unless (p e'')
            (expectationFailure
              ("predicate failed on expected exception: " ++ typ ++ " (" ++
                show e'' ++ ")"))
    Right a ->
      expectationFailure
        ("expected exception: " ++ typ ++ "\nbut got: " ++ show a)
  where
    typ :: String
    typ = show (typeOf (undefined :: e))

-- | A ShellDict that just logs its argument and succeeds.
logDict :: ShellDict
logDict = ShellDict { _shellCode = \s -> log s >> succeed }

succeed :: Applicative m => m (ExitCode, Text, Text)
succeed = pure (ExitSuccess, "", "")
