module Logging
  ( Tester
  , Message(..)
  , yieldLog
  )
where

import           Data.Word
import           Data.Time.Clock

import           Control.Monad.Writer.Lazy

import           Pipes

import           STLink

data Message
  = MessageFlipped { _flipAddr :: Word32
                   , _flipBefore :: Word8
                   , _flipAfter :: Word8
                   }
  | MessageLatchup
  | MessageReg { _regNum :: Word8
               , _regBefore :: Word32
               , _regAfter :: Word32
               }
  deriving (Show, Eq, Ord)

data LogMessage = LogMessage UTCTime Message
  deriving (Show, Eq, Ord)

data Log = Log UTCTime [LogMessage]
  deriving (Show, Eq, Ord)

type Tester = Producer LogMessage STLink

yieldLog :: Message -> Tester ()
yieldLog m = do
  time <- liftIO getCurrentTime
  yield $ LogMessage time m
