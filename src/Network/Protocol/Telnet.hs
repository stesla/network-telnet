{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module Network.Protocol.Telnet where

import           Control.Applicative
import           Data.Attoparsec.ByteString
import qualified Data.ByteString as B

import GHC.Word

data Chunk = Bytes B.ByteString
           | IAC Command
           deriving (Eq, Ord, Show)

data Command = Command Word8
             | LiteralIAC
             | WILL Option
             | WONT Option
             | DO Option
             | DONT Option
             deriving (Eq, Ord, Show)

newtype Option = Option Word8
               deriving (Eq, Ord, Show)

iac = word8 255

chunk = command <|> bytes

bytes = takeWhile1 (/=255) >>= pure . Bytes

command = word8 255 >> do
  willOption <|> wontOption <|> doOption <|> dontOption <|> literalIAC <|> command'
  where command' = anyWord8 >>= pure . IAC . Command

literalIAC = word8 255 >> pure (IAC LiteralIAC)
willOption = word8 251 >> anyWord8 >>= pure . IAC . WILL . Option
wontOption = word8 252 >> anyWord8 >>= pure . IAC . WONT . Option
doOption   = word8 253 >> anyWord8 >>= pure . IAC . DO   . Option
dontOption = word8 254 >> anyWord8 >>= pure . IAC . DONT . Option
