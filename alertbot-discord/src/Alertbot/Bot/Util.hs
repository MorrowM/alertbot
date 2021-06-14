module Alertbot.Bot.Util where

import           Calamity
import           Calamity.Commands
import           Calamity.Commands.Context (FullContext)
import           CalamityCommands.Check    (buildCheck, buildCheckPure)
import           Control.Lens
import           Control.Monad
import           Data.Flags
import           Data.Text                 (Text)
import qualified Data.Text.Lazy            as L
import           Data.Text.Lens
import qualified Di
import qualified DiPolysemy                as DiP
import qualified Polysemy                  as P
import qualified Polysemy.Fail             as P

-- | Create a `Check` for whether the user invoking the
-- command is an administrator.
isAdmin :: BotC r => P.Sem r (Check FullContext)
isAdmin = buildCheck "requires admin" $ \ctx -> do
  case ctx ^. #member of
    Nothing -> pure $ Just "This command can only be run in a server"
    Just mem -> do
      perms <- permissionsIn' (mem ^. #guildID) mem
      pure $ if perms `containsAll` administrator
        then Nothing
        else Just "User must be an administrator"

sameGuild :: (BotC r, P.Member P.Fail r) => FullContext -> GuildChannel -> P.Sem r ()
sameGuild ctx chan = when (Just (getID @Guild chan) == (getID <$> ctx ^. #guild)) $ do
        fire $ customEvt (CtxCommandError ctx $ CheckError "same guild" "Cannot modify buttons in other guilds")

inGuild :: Check FullContext
inGuild = buildCheckPure "In Server" $ \ctx ->
  case ctx ^. #guild of
    Just _  -> Nothing
    Nothing -> Just "This command can only be run inside a server."

handleFailByLogging :: P.Member (DiP.Di Di.Level Di.Path Di.Message) r => P.Sem (P.Fail : r) a -> P.Sem r ()
handleFailByLogging m = do
  r <- P.runFail m
  case r of
    Left e -> DiP.error @Text @Di.Path (e ^. packed)
    _      -> pure ()

info, debug :: BotC r => Text -> P.Sem r ()
info = DiP.info @Text @Di.Path
debug = DiP.info @Text @Di.Path

tellt_ :: (BotC r, Tellable t) => t -> Text -> P.Sem r ()
tellt_ tgt msg = void $ tell tgt msg

telllt_ :: (BotC r, Tellable t) => t -> L.Text -> P.Sem r ()
telllt_ tgt msg = void $ tell tgt msg

invoke_ :: _ => a -> P.Sem r ()
invoke_ x = void $ invoke x
