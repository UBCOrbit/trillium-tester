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
  Nothing -> pure ()
