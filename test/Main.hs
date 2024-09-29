
-- | Main test entry point.

module Main (main) where

import           Test.Tasty

import qualified DiskTest
import qualified VcrTest

tests :: TestTree
tests = testGroup "Tests"
    [ VcrTest.tests
    , DiskTest.tests
    ]

main :: IO ()
main = defaultMain tests

