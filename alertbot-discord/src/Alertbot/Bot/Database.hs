module Alertbot.Bot.Database where

import           Conduit
import           Control.Monad.Logger    (LoggingT, runStdoutLoggingT)
import           Data.Text               (Text)
import           Database.Persist.Sqlite
import qualified Polysemy                as P

type DatabaseAction a = SqlPersistT (LoggingT (ResourceT IO)) a

data Persistable m a where
  Db :: DatabaseAction a -> Persistable m a

P.makeSem ''Persistable

runPersistWith :: P.Member (P.Embed IO) r => Text -> P.Sem (Persistable : r) a -> P.Sem r a
runPersistWith conn = P.interpret $ \case
  Db action ->
    P.embed
      . runResourceT
      . runStdoutLoggingT
      . withSqliteConn conn
      . runSqlConn
      $ action
