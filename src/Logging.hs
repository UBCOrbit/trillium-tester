module Logging
  ( Tester
  , Message(..)
  , LogMessage
  , yieldLog
  , logStamped
  )
where

import           Data.Word
import           Data.Time.Clock
import           Data.Text                                ( Text )
import qualified Data.Text                     as T

import           Control.Monad.Writer.Lazy

import           Numeric                                  ( showHex
                                                          , showFFloat
                                                          )

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

hex :: (Integral a, Show a) => a -> Text
hex n = "0x" <> T.pack (showHex n "")

timestamp :: UTCTime -> UTCTime -> Text
timestamp start now =
  T.pack (showFFloat (Just 4) (realToFrac $ diffUTCTime now start :: Double) "")
    <> "s"

logMessage :: Message -> Text
logMessage (MessageFlipped a m m') =
  "flipped memory at " <> hex a <> " from " <> hex m <> " to " <> hex m'
logMessage (MessageReg a m m') =
  "flipped register " <> hex a <> " from " <> hex m <> " to " <> hex m'
logMessage MessageLatchup = "latched up core"

-- | A time-tagged message.
data LogMessage = LogMessage UTCTime Message
  deriving (Show, Eq, Ord)

logStamped :: UTCTime -> LogMessage -> Text
logStamped start (LogMessage t m) =
  "[" <> timestamp start t <> "] " <> logMessage m

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
