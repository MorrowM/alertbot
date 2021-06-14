module Alertbot.Bot.Commands where

import           Alertbot.Bot.Database
import           Alertbot.ReactNotify
import           Alertbot.WatchWords
import           Calamity
import           Calamity.Commands
import           Calamity.Commands.Context (FullContext)
import           CalamityCommands          (ConstructContext, ParsePrefix)
import           Control.Monad
import qualified Polysemy                  as P
import qualified Polysemy.Fail             as P

registerBotCommands ::
  ( BotC r
  , P.Members
    [ ParsePrefix Message
    , Persistable
    , P.Fail
    , ConstructContext Message FullContext IO ()
    ] r
  ) => P.Sem r ()
registerBotCommands = void $ addCommands @_ @FullContext $ do
  void helpCommand
  registerWatchWordCommands
  registerReactNotifyCommands

