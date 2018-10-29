module Main where

import Test.Tasty

import TestEncodings (testEncodings)
import TestEvent (testEvent)
import TestStreams (testStreams)
import TestDatabase (testDatabase)

import TestCmdArchive (testCmdArchive)

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ testGroup "Library"
        [ testEncodings
        , testEvent
        , testStreams
        , testDatabase
        ]
    , testGroup "Commands"
        [ testCmdArchive
        ]
    ]

