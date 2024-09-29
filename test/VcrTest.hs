{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | VCR unit and property tests.

module VcrTest where

import           Control.Monad
import           Control.Monad.Catch
import qualified Data.ByteString.Char8 as BS8
import           Data.Maybe
import           Pipes
import qualified Pipes.Prelude         as PP
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

import           TestCommon
import           Vcr

mkListPlayer :: MonadThrow m => [Event a] -> Player m Int (Event a)
mkListPlayer lst = Player
    { limits = pure (0, pred $ Prelude.length lst)
    , middle = \a b -> pure ((a + b) `div` 2)
    , peekItem = \ix -> pure (lst !! ix)
    , runPlayer = \direction ix flt -> do
        let lst' = Prelude.zip [0..] lst
            playAll = case direction of
                Forward  -> each (Prelude.drop ix lst')
                Backward -> each (Prelude.reverse $ Prelude.take (succ ix) lst')
        playAll >-> dashEvents' direction flt
    }

-- | Limits test
limitsTest :: TestTree
limitsTest = QC.testProperty "check limits" $ \utc0 -> do
    n <- choose (1, 100)
    events :: [Event ()] <- take n <$> genEvents ["ch1", "ch2"] 100 utc0
    let p :: Player Maybe Int (Event ())
        p = mkListPlayer events

        (i1, i2) = fromJust $ limits p
        a1 = fromJust $ peekItem p i1
        a2 = fromJust $ peekItem p i2

    pure
        (   ((i1, a1) === (0, head events))
       .&&. ((i2, a2) === (pred n, events !! pred n))
        )

-- | Next item test
nextItemTest :: TestTree
nextItemTest = QC.testProperty "check limits" $ \utc0 -> do
    n <- choose (2, 100)
    events :: [Event ()] <- take n <$> genEvents ["ch1", "ch2"] 100 utc0
    let p :: Player Maybe Int (Event ())
        p = mkListPlayer events

        results1 = do
            ix <- [0..pred (pred n)]
            let (ix', a') = fromJust $ nextItem p Forward ix Nothing
            pure ((succ ix, events !! succ ix) === (ix', a'))

        results2 = do
            ix <- [1..pred n]
            let (ix', a') = fromJust $ nextItem p Backward ix Nothing
            pure ((pred ix, events !! pred ix) === (ix', a'))

    pure (conjoin results1 .&&. conjoin results2)

-- | Basic bisection test - find equal elements.
bisectTest1 :: TestTree
bisectTest1 = QC.testProperty "bisect - find equal elements" $ \utc0 -> do
    n <- choose (1, 100)
    events :: [Event ()] <- take n <$> genEvents ["ch1", "ch2"] 100 utc0
    let p :: Player Maybe Int (Event ())
        p = mkListPlayer events
        isCorrect = do
            (i::Int, event) <- zip [0..] events
            let result = bisect p
                    (\event' -> compare (eTimeMono event') (eTimeMono event))
            pure $ result == Just (Just (i, event))
    pure (and isCorrect)

-- | Bisection test - find inside interval.
bisectTest2 :: TestTree
bisectTest2 = QC.testProperty "bisect - find in interval" $ forAll (choose (1,150)) $ \pVal -> do
    event :: Event Int <- arbitrary
    let eProbe = event { eValue = pVal }
        events :: [Event Int]
        events = do
            x <- [30,40..90]
            pure $ event { eValue = x }
        p :: Player Maybe Int (Event Int)
        p = mkListPlayer events
        result :: Maybe (Int, Event Int)
        result = fromJust $ bisect p (\x -> compare (eValue x) (eValue eProbe))
    pure $ if
        | pVal > 90 -> isNothing result
        | otherwise -> isJust result

-- | Bisection test - reindex
bisectTest3 :: TestTree
bisectTest3 = QC.testProperty "bisect - reindex" $ \utc0 -> do
    n <- choose (1, 100)
    events :: [Event ()] <- take n <$> genEvents ["ch1", "ch2"] 100 utc0
    let p :: Player Maybe Index (Event ())
        p = reindex $ mkListPlayer events
        isCorrect = do
            (i, event) <- zip (toIndex <$> [(0::Int)..]) events
            let result = bisect p
                    (\event' -> compare (eTimeMono event') (eTimeMono event))
            pure $ result == Just (Just (i, event))
    pure (and isCorrect)

-- | 'findEventByUtc' shall find any exact utc time from the ordered list.
findEventByUtcTest :: TestTree
findEventByUtcTest = QC.testProperty "findEventByUtcTest" $ \utc0 -> do
    n <- choose (1, 100)
    events :: [Event ()] <- take n <$> genEvents ["ch1", "ch2"] 100 utc0
    let p :: Player Maybe Int (Event ())
        p = mkListPlayer events
        isCorrect = do
            (i::Int, evt) <- zip [0..] events
            let result = fromJust $ findEventByTimeUtc p $ eTimeUtc evt
            pure $ result == Just (i, evt)
    pure $ and isCorrect

-- | Check if all events are present in replay.
playerListComplete :: TestTree
playerListComplete = QC.testProperty "list player" $ \utc0 direction -> do
    n <- choose (1, 100)
    events :: [Event ()] <- take n <$> genEvents ["ch1", "ch2"] 100 utc0
    let p :: Player Maybe Int (Event ())
        p = mkListPlayer events
        ix = case direction of
            Forward  -> 0
            Backward -> length events
        result = fromJust $ PP.toListM $ runPlayer p direction ix Nothing
        indexed = zip [0..] events
        expected = case direction of
            Forward  -> indexed
            Backward -> reverse indexed
    pure $ result == expected

validJSON :: TestTree
validJSON = QC.testProperty "valid json" $ \event ->
    let encoded = encodeJSON (event :: Event ())
    in conjoin
        [ BS8.notElem '\n' encoded
        , decodeJSON encoded == Just event
        ]

playerSteps :: TestTree
playerSteps = testCaseSteps "player" $ \step -> do

    samples :: [Event ()] <- do
        n <- getPositive <$> generate arbitrary
        t0 <- generate arbitrary
        generate $ take n <$> genEvents ["ch1", "ch2"] 100 t0

    let player :: Player Maybe Int (Event ())
        player = mkListPlayer samples

    step "replay forward/backward"
    do
        let (a, b) = fromJust $ limits player
            result1 = fromJust $ PP.toListM $ runPlayer player Forward a Nothing
            result2 = fromJust $ PP.toListM $ runPlayer player Backward b Nothing

        assertEqual "complete1" (length samples) (length result1)
        assertEqual "complete2" (length samples) (length result2)
        assertBool "reversed" (result1 == reverse result2)

        forM_ (zip samples result1) $ \(x, (i, y)) -> do
            assertBool "index" (i >= a)
            assertBool "index" (i <= b)
            assertEqual "readback" x y

propTests :: TestTree
propTests = testGroup "Property tests"
    [ limitsTest
    , nextItemTest
    , bisectTest1
    , bisectTest2
    , bisectTest3
    , findEventByUtcTest
    , playerListComplete
    , validJSON
    ]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
    [ playerSteps
    ]

tests :: TestTree
tests = testGroup "VcrTest" [propTests, unitTests]

main :: IO ()
main = defaultMain tests
