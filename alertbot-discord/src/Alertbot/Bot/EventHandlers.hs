module Alertbot.Bot.EventHandlers where

import           Alertbot.Bot.Config
import           Alertbot.Bot.Database
import           Alertbot.Bot.Util
import           Alertbot.ReactNotify
import           Alertbot.WatchWords
import           Calamity
import           Calamity.Commands
import           Calamity.Commands.Context (FullContext)
import           Control.Lens
import           Control.Monad
import qualified Data.Text.Lazy            as L
import qualified Polysemy                  as P
import qualified Polysemy.Fail             as P
import qualified Polysemy.Reader           as P
import           TextShow

registerEventHandlers ::
  ( BotC r
  , P.Members
   '[ P.Reader Config
    , Persistable
    ] r
  ) => P.Sem r ()
registerEventHandlers = do
  checkWatchWords
  registerCommandResponseHandler
  reactionHandler

registerCommandResponseHandler ::
  ( BotC r
  , P.Members
   '[ P.Reader Config
    ] r
  ) => P.Sem r ()
registerCommandResponseHandler = void . P.runFail $ do
  void $ react @('CustomEvt (CtxCommandError FullContext)) $ \(CtxCommandError ctx e) -> do
    info $ "Command failed with reason: " <> showt e
    void $ case e of
      ParseError n r -> tell @L.Text ctx $ "Failed to parse parameter: `" <> L.fromStrict n <> "`, with reason: ```\n" <> r <> "```"
      CheckError n r -> tell @L.Text ctx $ "Failed to pass a check: `" <> L.fromStrict n <> "`, with reason: ```\n" <> r <> "```"
      InvokeError n r -> tell @L.Text ctx $ "Failed to invoke command `" <> L.fromStrict n <> "`, with reason: ```\n" <> r <> "```"
    let msg = ctx ^. #message
    void . invoke $ CreateReaction msg msg (UnicodeEmoji "❌")

  void $ react @('CustomEvt (CommandInvoked FullContext)) $ \(CommandInvoked ctx) -> do
    emoj <- P.asks @Config $ view #reactPositiveEmoji
    let msg = ctx ^. #message
    void . invoke $ CreateReaction msg msg emoj
