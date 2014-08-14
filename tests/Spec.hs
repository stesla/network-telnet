{-# LANGUAGE OverloadedStrings #-}

module Main where

import Instances ()

import qualified Data.ByteString.Lazy as L

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
      readChunks "\255\255" `shouldBe` [Bytes (L.pack [255])]

    prop "reads arbitrary bytes (w/o telnet commands)" $
      let bytes = arbitrary `suchThat` \xs ->
            L.length xs > 0 && case L.elemIndex 255 xs of
              Nothing -> True
              Just x -> x > 0
          test xs = case readChunk xs of
            Just (Bytes bs, rest) -> L.append bs rest == xs
            _ -> False
        in forAll bytes test

    prop "reads arbitrary telnet commands" $ do
      let bytes = arbitrary `suchThat` \xs -> L.length xs >= 2 && (L.index xs 0) /= 255
          test xs = case readChunk (L.cons 255 xs) of
            Nothing -> False
            Just chunk ->
              case fst chunk of
                Command _ -> True
                WILL _ -> True
                WONT _ -> True
                DO _ -> True
                DONT _ -> True
                _ -> False
        in forAll bytes test

    describe "reads options" $do
      let test opt f x = case readChunk (L.pack [255, opt, x]) of
            Nothing -> False
            Just chunk ->
              case fst chunk of
                Bytes _ -> False
                Command _ -> False
                command -> command == f x
      prop "WILL" $ test 251 WILL
      prop "WONT" $ test 252 WONT
      prop "DO"   $ test 253 DO
      prop "DONT" $ test 254 DONT
