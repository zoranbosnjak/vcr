{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}

module VcrTest where

import           Control.Monad
import           Data.Functor.Identity
import           Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8

import           Pipes
import qualified Pipes.Prelude as PP

import           Test.Tasty
import           Test.Tasty.QuickCheck as QC
import           Test.Tasty.HUnit

import           Vcr
import           Common

import           TestCommon

-- | Basic bisection test - equal elements.
bisectTest1 :: TestTree
bisectTest1 = QC.testProperty "bisect - find equal elements" $ \n ->
    let lst :: [Int]
        lst = [0..pred (getPositive n)]
        isCorrect = do
            (i::Int, val) <- zip [0..] lst
            let result = runIdentity $ bisect lst (\x -> compare x val)
            return $ result == Just i
    in and isCorrect

-- | Bisection test - find inside interval.
bisectTest2 :: TestTree
bisectTest2 = QC.testProperty "bisect - find in interval" $ forAll (choose (1,150)) $ \p ->
    let lst :: [Int]
        lst = [30,40..90]
        result :: Maybe Int
        result = runIdentity $ bisect lst (\x -> compare x p)
    in
        if
            | p < 30 || p > 90 -> isNothing result
            | otherwise -> isJust result

-- | findEventByUtc shall find any exact utc time from the ordered list.
findEventByUtcTest :: TestTree
findEventByUtcTest = QC.testProperty "findEventByUtcTest" $ \e0 n ->
    let events :: [Event ()]
        events = take n $ PP.toList $ eventGenAddTime (1000*1000*100) e0
        isCorrect = do
            (i::Int, evt) <- zip [0..] events
            let result = runIdentity $ findEventByTimeUtc events $ eTimeUtc evt
            return $ result == Just i
    in and isCorrect

rawPlayerListComplete :: TestTree
rawPlayerListComplete = QC.testProperty "list player" $ \samples direction ->
    let player :: Direction -> Int -> Producer (Int, ()) Identity Int
        player = mkRawPlayer samples
        (result, ix) = runIdentity $ PP.toListM' $ player direction $ case direction of
            Backward -> length samples
            Forward -> 0
        indexed = zip [0..] samples
        expected = case direction of
            Backward -> (reverse indexed, 0)
            Forward -> (indexed, length samples)
    in
        (result, ix) == expected

validJSON :: TestTree
validJSON = QC.testProperty "valid json" $ \event ->
    let encoded = encodeJSON (event :: Event ())
    in conjoin
        [ BS8.notElem '\n' encoded
        , decodeJSON encoded == event
        ]

rawPlayerSteps :: TestTree
rawPlayerSteps = testCaseSteps "rawPlayer" $ \step -> do
    samples :: [BS.ByteString] <- generate $ listOf1 $ genLine $ const True

    let player :: Direction -> Int -> Producer (Int, BS.ByteString) Identity Int
        player = mkRawPlayer samples

    step "replay forward/backward"
    do
        Just (a, b) <- limits samples

        let (result1, ix1) = runIdentity $ PP.toListM' $ player Forward a
            (result2, ix2) = runIdentity $ PP.toListM' $ player Backward b

        assertEqual "final index1" b ix1
        assertEqual "final index2" a ix2
        assertEqual "complete" (length samples) (length result1)
        assertBool "reversed" (result1 == reverse result2)

        forM_ (zip samples result1) $ \(x, (i, y)) -> do
            assertBool "index" (i >= a)
            assertBool "index" (i <= b)
            assertEqual "readback" x y

propTests :: TestTree
propTests = testGroup "Property tests"
    [ bisectTest1
    , bisectTest2
    , findEventByUtcTest
    , rawPlayerListComplete
    , validJSON
    ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ rawPlayerSteps
    ]

tests :: TestTree
tests = testGroup "VcrTest" [propTests, unitTests]

main :: IO ()
main = defaultMain tests

