{-# OPTIONS -fno-warn-orphans #-}

module Instances where

import Control.Applicative ((<$>))
import qualified Data.ByteString as BS
import Test.QuickCheck (Arbitrary, arbitrary, shrink)

instance Arbitrary BS.ByteString where
  arbitrary = BS.pack <$> arbitrary
  shrink xs = BS.pack <$> shrink (BS.unpack xs)
