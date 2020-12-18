{-# OPTIONS_GHC -fno-warn-orphans #-}

module TestCommon where

import           Data.Word8
import qualified Data.ByteString as BS
import           Data.Time

import           Test.Tasty.QuickCheck as QC

import           Time

instance Arbitrary UTCTime where
    arbitrary = arbitraryUtc

-- | Line generator.
genLine :: (Word8 -> Bool) -> Gen BS.ByteString
genLine predicate = do
    s <- infiniteList
    n <- getSize
    m <- choose (0, n)
    return $ BS.pack $ take m $ filter predicate s

-- | Non-empty line generator.
genLine1 :: (Word8 -> Bool) -> Gen BS.ByteString
genLine1 predicate = genLine predicate `suchThat` (\x -> BS.length x > 0)

