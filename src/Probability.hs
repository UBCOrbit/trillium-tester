{-# LANGUAGE RecordWildCards #-}

module Probability
  ( multiTimer
  )
where

import           Data.Random.Normal

import           Control.Monad.Random
import           Control.Concurrent                hiding ( yield )

import           Pipes
import           Pipes.Concurrent

import           Options

genNormal
  :: (MonadSplit g m, RandomGen g, Random a, Floating a) => (a, a) -> m a
genNormal s = do
  gen <- getSplit
  pure . fst $ normal' s gen

multiTimer :: MonadIO m => [(UpsetConfig, Producer a m ())] -> Producer a m ()
multiTimer l = do
  (outbox, inbox) <- liftIO $ spawn unbounded
  let pipes = f outbox <$> l
  _ <- lift $ traverse (liftIO . forkIO . runEffect) pipes
  for (fromInput inbox) id
  where f o (c, a) = singleTimer c a >-> toOutput o

singleTimer :: MonadIO m => UpsetConfig -> a -> Producer a m ()
singleTimer c@UpsetConfig {..} a = do
  delay <- liftIO $ genNormal (_upsetDelayMean, _upsetDelayStddev)
  liftIO $ threadDelay (floor (delay * 1000))
  yield a
  singleTimer c a
