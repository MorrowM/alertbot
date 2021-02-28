module Discord.Alertbot.Commands where

import Calamity
import Calamity.Commands
import Control.Lens
import Control.Monad
import Data.Char
import Data.Foldable
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import Database.Persist
import Discord.Alertbot.Database
import Discord.Alertbot.Schema
import qualified Polysemy as P

registerBotCommands :: (BotC r, P.Member ParsePrefix r, P.Member Persistable r) => P.Sem r (P.Sem r (), CommandHandler, ())
registerBotCommands = addCommands $ do
  void helpCommand
  group "words" $ do
    void $
      command @'[[Text]] "add" $ \ctx wrds -> do
        Just gid <- pure $ getID <$> ctx ^. #guild
        res <- db $ traverse (insertUnique . WatchWord (getID $ ctx ^. #user) gid . T.map toLower) wrds
        void $
          tell @Text ctx $
            if any isNothing res
              then "Words added successfully (one or more already exist, however)"
              else "Words added successfully"

    void $
      command @'[[Text]] "remove" $ \ctx wrds -> do
        Just gid <- pure $ getID <$> ctx ^. #guild
        db $ traverse_ (deleteBy . UserGuildText (getID $ ctx ^. #user) gid . T.map toLower) wrds
        void $
          tell @Text ctx "Words removed successfully"

    void $
      command @'[] "list" $ \ctx -> do
        Just gid <- pure $ getID <$> ctx ^. #guild
        let getWord (WatchWord _ _ wrd) = wrd
        wrds <- db $ map (getWord . entityVal) <$> selectList [WatchWordGuild ==. gid, WatchWordUser ==. getID (ctx ^. #user)] [Asc WatchWordText]
        void $
          tell @Text ctx $
            if null wrds
              then "You have no watch words."
              else "Your words are:\n" <> T.intercalate ", " wrds

    void $
      command @'[] "clear" $ \ctx -> do
        Just gid <- pure $ getID <$> ctx ^. #guild
        db $ deleteWhere [WatchWordGuild ==. gid, WatchWordUser ==. getID (ctx ^. #user)]
        void $
          tell @Text ctx "Words cleared successfully"
