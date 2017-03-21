module Main where

import Test.Framework (defaultMain)

import TestEvent (testEvent)

main :: IO ()
main = defaultMain tests where
    tests =
        [ testEvent
        ]

