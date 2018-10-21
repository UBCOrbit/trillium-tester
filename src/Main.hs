{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  )
where

import           Options
import           Options.Applicative
import           Data.Yaml                                ( encode )
import qualified Data.ByteString               as BS

main :: IO ()
main = execParser argsInfo >>= \Args {..} -> case _argWriteConfig of
  Just f  -> BS.writeFile f (encode defaultConfig)
  Nothing -> getConfig _argConfigFile >>= print

getConfig :: Maybe FilePath -> IO Config
getConfig Nothing = undefined
getConfig (Just f) = undefined

startGenerators :: IO ()
startGenerators = pure ()
