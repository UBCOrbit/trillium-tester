module Main
  ( main
  )
where

import           Options
import           Options.Applicative

main :: IO ()
main = execParser argsInfo >>= print
