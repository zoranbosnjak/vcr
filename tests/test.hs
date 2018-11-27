module Main where

import Test.Tasty

import TestEncodings (testEncodings)
import TestEvent (testEvent)
import TestPipes (testPipes)
import TestDatabase (testDatabase)

import TestCmdArchive (testCmdArchive)

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ testGroup "Library"
        [ testEncodings
        , testEvent
        , testPipes
        , testDatabase
        ]
    , testGroup "Commands"
        [ testCmdArchive
        ]
    ]

