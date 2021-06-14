module Alertbot.Bot.Config where

import           Calamity
import           Data.Aeson
import           Data.Generics.Labels ()
import           GHC.Generics
import           Options.Generic

-- | CLI options
newtype CLIOptions w = Options
  { config :: w ::: FilePath <?> "The location of the json configuration file" <!> "bot.json"
  } deriving stock Generic

instance ParseRecord (CLIOptions Wrapped)

-- | The application configuration
data Config = Config
  { botToken           :: Text
  , reactPositiveEmoji :: RawEmoji
  , commandPrefix      :: Text
  , connectionString   :: Text
  , pkToken            :: Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (FromJSON, ToJSON)

