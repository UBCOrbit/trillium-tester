{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  )
where

import           Options
import           Options.Applicative

import           Data.Yaml                                ( decodeThrow
                                                          , decodeFileThrow
                                                          )
import qualified Data.ByteString               as BS

-- import Control.Concurrent
import Pipes

import Probability
import Upset
import STLink

main :: IO ()
main = execParser argsInfo >>= \Args {..} -> case _argWriteConfig of
  Just f  -> BS.writeFile f defaultConfig
  Nothing -> getConfig _argConfigFile >>= startGenerators

getConfig :: Maybe FilePath -> IO Config
getConfig Nothing  = decodeThrow defaultConfig
getConfig (Just f) = decodeFileThrow f

startGenerators :: Config -> IO ()
startGenerators Config {..} = do
  let pipeline = upsetGenerator latchup _configLatchup `for` (liftIO . print)
  void . withAutoBoard $ do
    enterMode DebugSWD
    runEffect pipeline
