{-# LANGUAGE GeneralizedNewtypeDeriving, NamedFieldPuns, OverloadedStrings #-}

module Network.Protocol.Telnet.Internal where

import Control.Arrow
import qualified Data.Map as M

import qualified Data.ByteString.Lazy as L
import GHC.Word

data Chunk = Bytes L.ByteString
           | Command Word8
           | Opt Opt
           deriving (Eq, Show)

data Opt = WILL Word8
         | WONT Word8
         | DO Word8
         | DONT Word8
         deriving (Eq, Show)

getOpt (WILL c) = c
getOpt (WONT c) = c
getOpt (DO   c) = c
getOpt (DONT c) = c

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

readOption :: (Word8 -> Opt) -> L.ByteString -> Maybe (Chunk, L.ByteString)
readOption f bs = L.uncons bs >>= \(byte, bs') -> Just (Opt (f byte), bs')

-- http://tools.ietf.org/html/rfc1143

data Option = Option { code :: Word8
                     , us :: OptionState
                     , usq :: OptionQueue
                     , them :: OptionState
                     , themq :: OptionQueue}
            deriving (Show)

data OptionState = No | WantNo | WantYes | Yes
                 deriving (Eq, Ord, Show)
data OptionQueue = Empty | Opposite
                 deriving (Eq, Ord, Show)
type OptionTable = M.Map Word8 Option

lookupOption :: Word8 -> OptionTable -> Option
lookupOption c table = case M.lookup c table of
  Nothing -> Option c No Empty No Empty
  Just opt -> opt

updateOption :: Opt -> OptionTable -> (OptionTable, Maybe Opt)
updateOption opt table = (M.update (\_ -> Just o') c table, opt')
  where
    c = getOpt opt
    o = lookupOption c table
    (o', opt') = case opt of
      WILL _ -> doWill
      WONT _ -> doWont
      DO   _ -> doDo
      DONT _ -> doDont
    doWill = case (them o, themq o) of
      (No      , _       ) -> (o                           , Just (DONT c))
      (Yes     , _       ) -> (o                           , Nothing)
      (WantNo  , Empty   ) -> (o {them=No                 }, Nothing)
      (WantNo  , Opposite) -> (o {them=Yes, themq=Empty   }, Nothing)
      (WantYes , Empty   ) -> (o {them=Yes                }, Nothing)
      (WantYes , Opposite) -> (o {them=WantNo, themq=Empty}, Just (DONT c))
    doWont = undefined
    doDo = undefined
    doDont = undefined
