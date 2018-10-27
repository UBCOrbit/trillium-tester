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

import           Pipes
import           STLink
import           Probability
import           Upset

main :: IO ()
main = execParser argsInfo >>= \Args {..} -> case _argWriteConfig of
  Just f  -> BS.writeFile f defaultConfig
  Nothing -> getConfig _argConfigFile >>= startGenerators

getConfig :: Maybe FilePath -> IO Config
getConfig Nothing  = decodeThrow defaultConfig
getConfig (Just f) = decodeFileThrow f

startGenerators :: Config -> IO ()
startGenerators Config {..} = do
  let pipe = multiTimer gens
  _ <- withAutoBoard $
    runEffect $ for pipe (liftIO . print)
  pure ()
  where gens = [(_configLatchup, latchup), (_configRegFlip, flipBitInReg)]
