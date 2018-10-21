{-# LANGUAGE QuasiQuotes #-}

module Options
  ( UpsetConfig(..)
  , Args(..)
  , Config(..)
  , argsInfo
  , defaultConfig
  )
where

import           Data.Default
import           Data.Yaml                         hiding ( Parser )
import           Data.Yaml.TH                             ( yamlQQ )
import           Data.Semigroup                           ( (<>) )
import qualified Data.ByteString               as BS

import           Options.Applicative

data UpsetConfig
  = UpsetConfig { _upsetEnable :: Bool
                , _upsetDelayMean :: Double
                , _upsetDelayStddev :: Double
                }
  deriving (Eq, Show, Ord)

instance Default UpsetConfig where
  def = UpsetConfig { _upsetEnable = True
                    , _upsetDelayMean = 10000
                    , _upsetDelayStddev = 2000
                    }

instance FromJSON UpsetConfig where
  parseJSON = withObject "upset configuration" $ \v ->
    UpsetConfig <$>
    v .: "enable" <*>
    v .: "delay" <*>
    v .: "stddev"

data Config
  = Config { _configLatchup :: UpsetConfig
           , _configMemFlip :: UpsetConfig
           , _configRegFlip :: UpsetConfig
           }
  deriving (Show, Eq, Ord)

instance FromJSON Config where
  parseJSON = withObject "configuration" $ \v ->
    v .: "upsets" >>= withObject "upset list" f
    where
      f o = Config <$>
            o .: "latchup" <*>
            o .: "memflip" <*>
            o .: "regflip"

defaultConfig :: BS.ByteString
defaultConfig = encode [yamlQQ|
# all delays are in ms
upsets:
  latchup:
    enable: false
    delay: 600000
    stddev: 2000
  memflip:
    enable: true
    delay: 100
    stddev: 10
  regflip:
    enable: true
    delay: 1000
    stddev: 100
|]

data Args
  = Args { _argConfigFile :: Maybe FilePath
         , _argRunTime :: Maybe Int
         , _argWriteConfig :: Maybe FilePath
         }
  deriving (Eq, Show, Ord)

args :: Parser Args
args =
  Args
    <$> optional
          (strOption
            (long "config" <> short 'c' <> metavar "CONFIG" <> help
              "config file to read settings from"
            )
          )
    <*> optional
          (option
            auto
            (long "timeout" <> short 't' <> metavar "SECONDS" <> help
              "exit after given number of seconds"
            )
          )
    <*> optional
          (strOption
            (long "write-config" <> short 'w' <> metavar "FILE" <> help
              "dump the default config to the specified file"
            )
          )

argsInfo :: ParserInfo Args
argsInfo =
  info (args <**> helper)
    $  fullDesc
    <> progDesc
         "Simulates radiation upsets on STM32s through the \
           \STLink debug interface.  By the UBC Orbit CDH Team."
    <> header "trillium - Simulate radiation upsets"
