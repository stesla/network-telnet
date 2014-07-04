{-# LANGUAGE TemplateHaskell #-}
module Main where

import Test.QuickCheck
import Test.QuickCheck.All

import System.Environment

import Control.Applicative
import Control.Monad

import qualified Data.ByteString as B

import Network.Protocol.Telnet

main :: IO ()
main = do
    n <- length . filter not <$> mapM perform tests
    unless (n == 0) (error (show n ++ " test(s) failed"))
  where
    perform f = do { f }
    tests = [$quickCheckAll]

instance Arbitrary B.ByteString where
    arbitrary = fmap B.pack arbitrary

prop_readOnlyBytes = forAll plainBytes prop
  where prop bs = B.length bs > 0 ==> readChunk bs == Just (Bytes bs, B.empty)
        plainBytes = suchThat arbitrary (\x -> B.elemIndex 255 x == Nothing)
