{-# LANGUAGE RecordWildCards #-}

module Probability
  ()
where

import           Data.Random.Normal

import           Control.Monad.Random
import           Control.Concurrent

import           Upset
import           Options
import           Logging

genNormal
  :: (MonadSplit g m, RandomGen g, Random a, Floating a) => (a, a) -> m a
genNormal s = do
  gen <- getSplit
  pure . fst $ normal' s gen

upsetGenerator :: Tester () -> UpsetConfig -> Tester ()
upsetGenerator upset c@(UpsetConfig {..}) = when _upsetEnable $ do
  delay <- liftIO $ genNormal (_upsetDelayMean, _upsetDelayStddev)
  liftIO $ threadDelay (floor (delay * 1000))
  upset
  upsetGenerator upset c
