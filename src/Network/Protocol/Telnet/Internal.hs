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
           | End
           deriving (Eq, Show)

readChunks :: L.ByteString -> [Chunk]
readChunks bs = if L.null bs || chunk == End then [] else chunk : readChunks bs'
  where (chunk, bs') = readChunk bs

readChunk :: L.ByteString -> (Chunk, L.ByteString)
readChunk bs = case L.uncons bs of
  Nothing -> (End, L.empty)
  Just (255, bs') -> readCommand bs'
  Just _ -> readBytes bs

readBytes :: L.ByteString -> (Chunk, L.ByteString)
readBytes = first Bytes . L.span (/= 255)

readCommand :: L.ByteString -> (Chunk, L.ByteString)
readCommand bs = case L.uncons bs of
  Nothing -> (End, L.empty)
  Just (255, bs') -> (Bytes $ L.pack [255], bs')
  Just (w, bs') | 251 <= w && w <= 254 ->
    case L.uncons bs' of
      Nothing -> (End, L.empty)
      Just (o, bs'') ->
        case w of
          251 -> (WILL o, bs'')
          252 -> (WONT o, bs'')
          253 -> (DO o, bs'')
          254 -> (DONT o, bs'')
          -- because of the guard above, we'll never hit this case
          _   -> undefined
  Just (w, bs') -> (Command w, bs')
