
-- | Main test entry point.

module Main (main) where

import           Test.Tasty

import qualified VcrTest
import qualified DiskTest

tests :: TestTree
tests = testGroup "Tests"
    [ VcrTest.tests
    , DiskTest.tests
    ]

main :: IO ()
main = defaultMain tests

