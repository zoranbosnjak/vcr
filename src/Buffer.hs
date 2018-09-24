----------------
-- |
-- Module:      Buffer
--
-- Buffer manipulations
--

{-# LANGUAGE DeriveGeneric #-}

module Buffer where

import           GHC.Generics (Generic)
import           Data.Monoid ((<>))
import qualified Options.Applicative as Opt
import           Data.Aeson (ToJSON, FromJSON)

import qualified Common as C

-- | Threshold, limit by different properties.
data Threshold = Threshold
    { thLength      :: Maybe Int
    , thBytes       :: Maybe Integer
    } deriving (Generic, Eq, Show)

instance ToJSON Threshold
instance FromJSON Threshold

instance Monoid Threshold where
    mempty = Threshold Nothing Nothing
    mappend = (<>)

instance Semigroup Threshold where
    Threshold a1 b1 <> Threshold a2 b2 = Threshold
        (lower a1 a2)
        (lower b1 b2)
      where
        lower Nothing x = x
        lower x Nothing = x
        lower (Just a) (Just b) = Just (min a b)

thresholdValid :: Threshold -> Bool
thresholdValid th = not (th == mempty)

thresholdOptions :: String -> Opt.Parser Threshold
thresholdOptions s = threshold
    <$> Opt.optional (Opt.option Opt.auto
        ( Opt.long (s++"Events")
       <> Opt.help "number of events")
        )
    <*> Opt.optional (Opt.option C.kiloMega
        ( Opt.long (s++"Bytes")
       <> Opt.help "size in bytes")
        )

threshold :: Maybe Int -> Maybe Integer -> Threshold
threshold = Threshold

