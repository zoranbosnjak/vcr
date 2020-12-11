module TestCommon where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Char8 as BS8

import           Test.Tasty.QuickCheck as QC

newtype TextLine = TextLine { getTextLine :: BS.ByteString }

instance Arbitrary TextLine where
    arbitrary = fmap (TextLine . BS.filter (/= BS.c2w '\n') . BS8.pack) arbitrary

