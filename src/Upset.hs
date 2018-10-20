module Upset
  ( flipBitInMem
  , flipBitInReg
  , latchup
  )
where

import           Data.Bits
import           Data.Word
import qualified Data.ByteString               as BS

import           Control.Monad.Random

import           STLink
import           Logging

-- | Flip one random bit in the given integer.
flipRandomBit :: (FiniteBits n, MonadRandom m) => n -> m n
flipRandomBit n = do
  b <- getRandomR (0, finiteBitSize n - 1)
  pure $ complementBit n b

-- | Pick an address in the given region of memory, and flip one
-- random bit in it.
flipBitInMem :: (Word32, Word32) -> Tester ()
flipBitInMem (lo, hi) = do
  a  <- liftIO $ getRandomR (lo, hi)
  b  <- lift $ BS.head <$> readMem a 1
  b' <- liftIO $ flipRandomBit b
  lift . writeMem a . BS.singleton $ b'
  yieldLog $ MessageFlipped a b b'

-- | Pick a register and flip a bit!
flipBitInReg :: Tester ()
flipBitInReg = do
  r  <- liftIO $ getRandomR (0, 15)
  n  <- lift $ readReg r
  n' <- liftIO $ flipRandomBit n
  lift $ writeReg r n'
  yieldLog $ MessageReg r n n'

-- | Cause a latchup.
latchup :: Tester ()
latchup = do
  lift haltCore
  yieldLog MessageLatchup
