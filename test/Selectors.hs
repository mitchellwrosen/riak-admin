module Selectors where

import Riak.Admin.Internal

import Test.Hspec.Expectations

anyShellFailure :: Selector ShellFailure
anyShellFailure = const True

shellFailureCode :: Int -> Selector ShellFailure
shellFailureCode n (ShellFailure _ m _ _) = n == m
shellFailureCode _ _ = False
