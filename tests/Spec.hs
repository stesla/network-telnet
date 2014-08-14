{-# LANGUAGE OverloadedStrings #-}

module Main where

import Instances ()

import qualified Data.ByteString.Lazy as B

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import Network.Protocol.Telnet.Internal

main :: IO ()
main = hspec $ do
  describe "readChunk" $ modifyMaxSuccess (\_ -> 1000) $ do
    describe "parsing fails" $ do
      let test x = readChunks x `shouldBe` []
      it "an empty string" $ test ""
      it "an incomplete command" $ test "\255"
      it "an incomplete option command" $ do
        test "\255\251"
        test "\255\252"
        test "\255\253"
        test "\255\254"

    it "reads literal IAC" $ do
      readChunks "\255\255" `shouldBe` [Bytes (B.pack [255])]

    prop "reads arbitrary bytes (w/o telnet commands)" $
      let bytes = arbitrary `suchThat` \xs ->
            B.length xs > 0 && case B.elemIndex 255 xs of
              Nothing -> True
              Just x -> x > 0
          test xs = case readChunk xs of
            (Bytes bs, rest) -> B.append bs rest == xs
            _ -> False
        in forAll bytes test

    prop "reads arbitrary telnet commands" $ do
      let bytes = arbitrary `suchThat` \xs -> B.length xs >= 2 && (B.index xs 0) /= 255
          test xs = case fst $ readChunk (B.cons 255 xs) of
            Command _ -> True
            WILL _ -> True
            WONT _ -> True
            DO _ -> True
            DONT _ -> True
            _ -> False
        in forAll bytes test

    describe "reads options" $do
      let test opt f x = case fst $ readChunk (B.pack [255, opt, x]) of
            Bytes _ -> False
            Command _ -> False
            End -> False
            command -> command == f x
      prop "WILL" $ test 251 WILL
      prop "WONT" $ test 252 WONT
      prop "DO"   $ test 253 DO
      prop "DONT" $ test 254 DONT
