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
import           Data.Time.Clock
import qualified Data.ByteString               as BS
import qualified Data.Text.IO                  as T

import           Pipes
import           STLink
import           Probability
import           Upset
import           Logging

main :: IO ()
main = execParser argsInfo >>= \Args {..} -> case _argWriteConfig of
  Just f  -> BS.writeFile f defaultConfig
  Nothing -> getConfig _argConfigFile >>= startGenerators

getConfig :: Maybe FilePath -> IO Config
getConfig Nothing  = decodeThrow defaultConfig
getConfig (Just f) = decodeFileThrow f

startGenerators :: Config -> IO ()
startGenerators Config {..} = do
  start <- getCurrentTime
  let pipe = multiTimer gens
  _ <- withAutoBoard $ do
    enterMode DebugSWD
    runEffect $ for pipe (liftIO . T.putStrLn . logStamped start)
  pure ()
  where gens = [(_configLatchup, latchup), (_configRegFlip, flipBitInReg)]
