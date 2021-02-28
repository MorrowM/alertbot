module Discord.Alertbot where

import Calamity
import Calamity.Cache.Eff
import Calamity.Cache.InMemory
import Calamity.Commands
import Calamity.Metrics.Noop
import Control.Lens
import Control.Monad
import Data.Coerce
import Data.Flags
import Data.Foldable
import Data.List
import qualified Data.Text as T
import Data.Text.Lazy (Text)
import Data.Text.Lens
import qualified Database.Persist as DB
import qualified Database.Persist.Sql as DB
import qualified Di
import qualified DiPolysemy as DiP
import Discord.Alertbot.Database
import Discord.Alertbot.Schema
import qualified Polysemy as P
import qualified Polysemy.Fail as P
import System.Environment
import TextShow

import Discord.Alertbot.Commands
import GHC.Generics

showtl :: Show a => a -> Text
showtl = view packed . show

info, debug :: BotC r => Text -> P.Sem r ()
info = DiP.info @Text @Di.Path
debug = DiP.info @Text @Di.Path

data Settings = Settings
  { commandPrefix :: Text,
    connectionString :: T.Text
  }
  deriving (Eq, Show, Generic)

handleFailByLogging :: P.Member (DiP.Di Di.Level Di.Path Di.Message) r => P.Sem (P.Fail : r) a -> P.Sem r ()
handleFailByLogging m = do
  r <- P.runFail m
  case r of
    Left e -> DiP.error @Text @Di.Path (e ^. packed)
    _ -> pure ()

runBotWith :: Settings -> IO ()
runBotWith settings = do
  tok <- view packed <$> getEnv "BOT_TOKEN"
  Di.new $ \di ->
    void
      . P.runFinal
      . P.embedToFinal @IO
      . DiP.runDiToIO di
      . runCacheInMemory
      . runMetricsNoop
      . runPersistWith (settings ^. #connectionString)
      . useConstantPrefix (settings ^. #commandPrefix)
      $ runBotIO (BotToken tok) defaultIntents $ do
        db $ DB.runMigration migrateAll
        void registerBotCommands 
        react @'MessageCreateEvt $ \msg -> handleFailByLogging $ do
          Just self <- getBotUser
          when (self ^. #id /= msg ^. #author) $ do
            Just g <- pure $ msg ^. #guildID
            wrds <- fmap (map DB.entityVal) $ db $ DB.selectList [WatchWordGuild DB.==. g] []
            let wrdMatches = filter ((`T.isInfixOf` (T.toCaseFold $ msg ^. #content . strict)) . T.toCaseFold . getText) wrds
                usersToPing = coerce $ nub $ map watchWordUser wrdMatches :: [Snowflake User]
            for_ usersToPing $ \usr -> do
              perms <- (coerce $ msg ^. #channelID :: Snowflake GuildChannel) `permissionsIn'` usr
              when (usr /= (msg ^. #author) && (perms `containsAll` viewChannel)) $ do
                Just auth <- upgrade $ msg ^. #author
                void $
                  tell @T.Text usr $
                    (auth ^. #username . strict) <> " mentioned a watch word in their message:\n> "
                      <> T.take 300 (msg ^. #content . strict)
                      <> "\n"
                      <> messageUrl msg g

messageUrl :: Message -> Snowflake Guild -> T.Text
messageUrl msg g = "https://discord.com/channels/" <> showt g <> "/" <> showt msg

getText :: WatchWord -> T.Text
getText (WatchWord _ _ txt) = txt

defaultSettings :: Settings
defaultSettings =
  Settings
    { commandPrefix = "!",
      connectionString = "alertbot.sqlite"
    }

runBot :: IO ()
runBot = runBotWith defaultSettings