{-# OPTIONS -fno-warn-orphans #-}

module Instances where

import Control.Applicative ((<$>))
import qualified Data.ByteString.Lazy as L
import Test.QuickCheck (Arbitrary, arbitrary, shrink)

instance Arbitrary L.ByteString where
  arbitrary = L.pack <$> arbitrary
  shrink xs = L.pack <$> shrink (L.unpack xs)
