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
             | WILL Option
             | WONT Option
             | DO Option
             | DONT Option
             deriving (Eq, Ord, Show)

newtype Option = Option Word8
               deriving (Eq, Ord, Show)

willOption = word8 251 >> anyWord8 >>= pure . IAC . WILL . Option
wontOption = word8 252 >> anyWord8 >>= pure . IAC . WONT . Option
doOption   = word8 253 >> anyWord8 >>= pure . IAC . DO   . Option
dontOption = word8 254 >> anyWord8 >>= pure . IAC . DONT . Option
