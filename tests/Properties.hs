{-# LANGUAGE OverloadedStrings #-}

module Main where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Control.Applicative

import Network.Protocol.Telnet

import qualified Data.ByteString as B

main :: IO ()
main = hspec $ do
  describe "readChunk" $ modifyMaxSuccess (\_ -> 1000) $ do
    describe "parsing fails" $ do
      let test x = readChunk x `shouldBe` Nothing
      it "an empty string" $ test ""
      it "an incomplete command" $ test "\255"
      it "an incomplete option command" $ do
        test "\255\251"
        test "\255\252"
        test "\255\253"
        test "\255\254"

    it "reads literal IAC" $ do
      readChunk "\255\255" `shouldBe` Just (Bytes $ B.pack [255], B.empty)

    prop "reads arbitrary bytes (w/o telnet commands)" $
      let bytes = arbitrary `suchThat` \xs ->
            B.length xs > 0 && case B.elemIndex 255 xs of
              Nothing -> True
              Just x -> x > 0
          test xs = case readChunk xs of
            Just (Bytes bs, rest) -> B.append bs rest == xs
            _ -> False
        in forAll bytes test

    prop "reads arbitrary telnet commands" $ do
      let bytes = arbitrary `suchThat` \xs -> B.length xs >= 2 && (B.index xs 0) /= 255
          test xs = case readChunk (B.cons 255 xs) of
            Just (IAC _, _) -> True
            _ -> False
        in forAll bytes test

    describe "reads options" $do
      let test opt f x = case readChunk (B.pack [255, opt, x]) of
            Just (IAC command, _) -> command == f (Option x)
            _ -> False
      prop "WILL" $ test 251 WILL
      prop "WONT" $ test 252 WONT
      prop "DO"   $ test 253 DO
      prop "DONT" $ test 254 DONT

instance Arbitrary B.ByteString where
  arbitrary = B.pack <$> arbitrary
  shrink xs = B.pack <$> shrink (B.unpack xs)
