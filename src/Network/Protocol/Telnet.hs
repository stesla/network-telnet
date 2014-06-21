{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings #-}
{-# OPTIONS -Wall -fno-warn-missing-signatures #-}

module Network.Protocol.Telnet where

import           Control.Applicative
import           Control.Arrow (first)
import           Data.Attoparsec.ByteString hiding (parse)
import qualified Data.Attoparsec.ByteString as A
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

parse :: B.ByteString -> ([Chunk], B.ByteString)
parse str = case B.elemIndex 255 str of
  Nothing -> if B.null str then ([], B.empty) else ([Bytes str], B.empty)
  _ -> case A.parse chunk str of
    Fail _ _ y    -> error y
    Done i c  -> first (c:) (parse i)
    p@(Partial _) ->
      case feed p "" of
        Fail _ _ _ -> ([], str)
        Done _ c'  -> ([c'], B.empty)
        Partial _  -> error "not reachable"

chunk = command <|> bytes

bytes = takeWhile1 (/=255) >>= pure . Bytes

command = word8 255 >> do
  word8 255 >> pure (IAC LiteralIAC)
  <|> (word8 251 >> anyWord8 >>= pure . IAC . WILL . Option)
  <|> (word8 252 >> anyWord8 >>= pure . IAC . WONT . Option)
  <|> (word8 253 >> anyWord8 >>= pure . IAC . DO   . Option)
  <|> (word8 254 >> anyWord8 >>= pure . IAC . DONT . Option)
  <|> (satisfy (not . isOption) >>= pure . IAC . Command)
  where isOption w = 251 <= w && w <= 254
