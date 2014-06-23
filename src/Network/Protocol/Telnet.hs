{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module Network.Protocol.Telnet where

import qualified Data.ByteString as B

import GHC.Word

data Chunk = Bytes B.ByteString
           | IAC Command
           deriving (Eq, Ord, Show)

data Command = Command Word8
             | WILL Option
             | WONT Option
             | DO Option
             | DONT Option
             deriving (Eq, Ord, Show)

newtype Option = Option Word8
               deriving (Eq, Ord, Show)

type ParseResult = Maybe (Chunk, B.ByteString)

readChunk :: B.ByteString -> ParseResult
readChunk str = case B.elemIndex 255 str of
  Nothing -> Just (Bytes str, B.empty)
  Just 0  -> readCommand $ B.tail str
  Just n  ->
    let (xs, str') = B.splitAt n str in
    case readCommand $ B.tail str' of
      Just (Bytes ys, str'') -> return (Bytes $ B.append xs ys, str'')
      _                      -> return (Bytes xs, str')

readCommand :: B.ByteString -> ParseResult
readCommand str = do
  (x, str') <- B.uncons str
  case x of
    251 -> readOption WILL str'
    252 -> readOption WONT str'
    253 -> readOption DO   str'
    254 -> readOption DONT str'
    255 -> case readChunk str' of
      Just (Bytes xs, str'') -> return (Bytes $ B.cons 255 xs, str'')
      _                      -> return (Bytes $ B.pack [255], str')
    _   -> return (IAC $ Command x, str')

readOption :: (Option -> Command) -> B.ByteString -> ParseResult
readOption f str = do
  (x, str') <-  B.uncons str
  return (IAC $ f $ Option x, str')
