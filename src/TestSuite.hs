module TestSuite where

import Data.Monoid (mempty)
import Test.Framework (defaultMain, defaultMainWithOpts, testGroup)
import Test.Framework.Options (TestOptions, TestOptions'(..))
import Test.Framework.Runners.Options (RunnerOptions, RunnerOptions'(..))
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Test.QuickCheck
import Test.HUnit

import Data.List

import Data.Word

import Data.SequenceNumber

main :: IO ()
main = defaultMain tests
-- do print ( (0::SeqNo32) - maxDistance - maxDistance )

mainWithOpts = do
  -- Test options can also be specified in the code. The TestOptions
  -- type is an instance of the Monoid type class, so the easiest way
  -- to get an empty set of options is with `mempty`.
  let empty_test_opts = mempty :: TestOptions

  -- We update the empty TestOptions with our desired values.
  let my_test_opts = empty_test_opts {
    topt_maximum_generated_tests = Just 500
  }

  -- Now we create an empty RunnerOptions in the same way, and add
  -- our TestOptions to it.
  let empty_runner_opts = mempty :: RunnerOptions
  let my_runner_opts = empty_runner_opts {
    ropt_test_options = Just my_test_opts
  }

  defaultMainWithOpts tests my_runner_opts

tests = [
  testGroup "Basic SeqNo8" [
     testProperty "successor distance 1" (prop_SuccessorDistance  :: SeqNo8 -> Bool)
   , testProperty "predecessor distance 1" (prop_PredecessorDistance :: SeqNo8 -> Bool)
   , testProperty "successor GTE" (prop_SuccessorGTE :: SeqNo8 -> Bool)
   , testProperty "successor GT" (prop_SuccessorGT :: SeqNo8 -> Bool)
   , testProperty "successor Not LTE" (prop_SuccessorNotLTE :: SeqNo8 -> Bool)
   , testProperty "successor Not LT" (prop_SuccessorNotLT:: SeqNo8 -> Bool)
            ]
        ]


prop_SuccessorDistance s
 = (s `distance` (succ s)) == 1

prop_PredecessorDistance s
 = (s `distance` (pred s)) == -1

prop_SuccessorGTE s 
 = (succ s >= s)

prop_SuccessorGT s
 = (succ s > s)

prop_SuccessorNotLTE s
 = not $ succ s <= s

prop_SuccessorNotLT s
 = not $ succ s < s

instance Arbitrary SeqNo8 where
  arbitrary = arbitraryBoundedIntegral

instance Arbitrary SeqNo16 where
  arbitrary = arbitraryBoundedIntegral

instance Arbitrary SeqNo32 where
  arbitrary = arbitraryBoundedIntegral

instance Arbitrary SeqNo64 where
  arbitrary = arbitraryBoundedIntegral