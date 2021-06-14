module Alertbot where

import           Alertbot.Bot.Commands
import           Alertbot.Bot.Config
import           Alertbot.Bot.Database
import           Alertbot.Bot.EventHandlers
import           Alertbot.Bot.Schema
import           Alertbot.Bot.Util
import           Calamity
import           Calamity.Cache.InMemory
import           Calamity.Commands          (useConstantPrefix)
import           Calamity.Commands.Context
import           Calamity.Metrics.Noop
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Flags
import qualified Database.Persist.Sql       as DB
import qualified Di
import qualified DiPolysemy                 as DiP
import           Options.Generic
import qualified Polysemy                   as P
import qualified Polysemy.Reader            as P

-- | Run the bot with a given configuration.
runBotWith :: Config -> IO ()
runBotWith cfg = Di.new $ \di ->
  void
  . P.runFinal
  . P.embedToFinal @IO
  . DiP.runDiToIO di
  . runCacheInMemory
  . useFullContext
  . runMetricsNoop
  . runPersistWith (cfg ^. #connectionString)
  . useConstantPrefix (cfg ^. #commandPrefix . lazy)
  . P.runReader cfg
  . runBotIO (BotToken (cfg ^. #botToken . lazy)) (defaultIntents .+. intentGuildMembers .+. intentGuildPresences)
  . handleFailByLogging $ do
    db $ DB.runMigration migrateAll
    registerBotCommands
    registerEventHandlers

-- | Run the bot in the `IO` monad, reading the configuration
-- from a `bot.json` file.
main :: IO ()
main = do
  opts <- unwrapRecord @_ @CLIOptions "Pandabot - a bot for pandas"
  mconfig <- eitherDecodeFileStrict (opts ^. #config)
  case mconfig of
    Left err  -> print err
    Right cfg -> runBotWith cfg
