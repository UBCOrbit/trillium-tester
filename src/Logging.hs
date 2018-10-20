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

-- | A message representing an event that the tool has caused.
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

-- | A time-tagged message.
data LogMessage = LogMessage UTCTime Message
  deriving (Show, Eq, Ord)

-- | A series of time-tagged messages preceded by the start time of
-- the program.
data Log = Log UTCTime [LogMessage]
  deriving (Show, Eq, Ord)

-- | Type synonym representing a pipe that can perform 'STLink' side
-- effects *and* produce log messages and send them down the pipe.
type Tester = Producer LogMessage STLink

-- | Produce a log message and send it down the pipe.
yieldLog :: Message -> Tester ()
yieldLog m = do
  time <- liftIO getCurrentTime
  yield $ LogMessage time m
