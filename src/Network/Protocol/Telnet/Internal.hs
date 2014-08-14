{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}

module Network.Protocol.Telnet.Internal where

import Control.Arrow

import qualified Data.ByteString.Lazy as L
import GHC.Word

data Chunk = Bytes L.ByteString
           | Command Word8
           | WILL Word8
           | WONT Word8
           | DO Word8
           | DONT Word8
           deriving (Eq, Show)

readChunks :: L.ByteString -> [Chunk]
readChunks bs =
  if L.null bs then [] else
    case readChunk bs of
      Nothing -> []
      Just (chunk, bs') -> chunk : readChunks bs'

readChunk :: L.ByteString -> Maybe (Chunk, L.ByteString)
readChunk bs = L.uncons bs >>= \(byte, bs') -> do
  case byte of
    255 -> readCommand bs'
    _ -> readBytes bs

readBytes :: L.ByteString -> Maybe (Chunk, L.ByteString)
readBytes = Just . first Bytes . L.span (/= 255)

readCommand :: L.ByteString -> Maybe (Chunk, L.ByteString)
readCommand bs = L.uncons bs >>= \(byte, bs') -> do
  case byte of
    251 -> readOption WILL bs'
    252 -> readOption WONT bs'
    253 -> readOption DO bs'
    254 -> readOption DONT bs'
    255 -> Just (Bytes $ L.pack [255], bs')
    _   -> Just (Command byte, bs')

readOption :: (Word8 -> Chunk) -> L.ByteString -> Maybe (Chunk, L.ByteString)
readOption f bs = L.uncons bs >>= \(byte, bs') -> Just (f byte, bs')
