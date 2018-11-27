
{-# LANGUAGE LambdaCase #-}

module TestDatabase (testDatabase) where

import Control.Monad
import System.IO.Temp
import Data.Foldable (toList)
import Data.Sequence as DS
import Data.List
import Control.Concurrent.STM

import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import Pipes
import Pipes.Safe

import Database
import Event

testDatabase :: TestTree
testDatabase = testGroup "Database" $ join
    [ probe 1 1
    , probe 1 2
    , probe 1 3
    , probe 2 1
    , probe 2 2
    , probe 2 3
    , probe 10 10
    , probe 100 100
    , probe 1000 1000
    , probe 10000 10000
    , probe 10000 10001
    , probe 10000 9999
    , probe 10000 100000
    , probe 1 10000
    , probe 10000 1
    ]
  where
    probe chunkSize totalSize =
        [ testCase
            ("write/read task   " ++ show chunkSize ++ ", " ++ show totalSize)
            $ verify totalSize (databaseWriterTask chunkSize)
        , testCase
            ("write/read stream " ++ show chunkSize ++ ", " ++ show totalSize)
            $ verify totalSize (databaseWriterProcess
                (\_ -> return ())   -- set status
                (Buffer chunkSize 1.0)    -- (thSend, timeout)
                (maxBound `div` 2)  -- thDrop
                (\_ -> return ())   -- onDrop
                )
        ]

verify :: Int -> (Db -> Consumer Event (SafeT IO) ()) -> Assertion
verify totalSize dbWriter = do
  withSystemTempFile "test.db" $ \fp _h -> do
    let db = DbSQLite3 fp

    prepareDatabase db

    orig <- Prelude.take totalSize <$> generate arbitrary

    -- write original events to database
    do
        let producer = each orig
            consumer = dbWriter db
        runSafeT $ runEffect $ producer >-> consumer

    -- read back from database
    readback <- do
        bufV <- newTVarIO DS.empty
        let consumer = forever $ do
                x <- await
                liftIO $ atomically $ modifyTVar bufV (DS.|> x)
        runSafeT $ runEffect $
            databaseReaderTask db Nothing Nothing [] [] >-> consumer
        fmap toList $ readTVarIO bufV

    -- verify length
    assertEqual "readback length"
        (Prelude.length orig) (Prelude.length readback)

    -- verify content
    assertEqual "readback content"
        True
        (Data.List.sort orig == Data.List.sort readback)

