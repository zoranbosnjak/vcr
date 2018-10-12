module Main where

import Test.Framework (defaultMain)

import TestEncodings (testEncodings)
import TestEvent (testEvent)

main :: IO ()
main = defaultMain tests where
    tests =
        [ testEncodings
        , testEvent
        ]

