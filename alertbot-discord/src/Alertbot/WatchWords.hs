module Alertbot.WatchWords where

import           Alertbot.Bot.Database
import           Alertbot.Bot.Schema
import           Alertbot.Bot.Util
import           Calamity
import           Calamity.Cache.Eff
import           Calamity.Commands.Context (FullContext)
import           Calamity.Commands.Dsl
import           Control.Lens
import           Control.Monad
import           Data.ByteString           (ByteString)
import           Data.Char
import           Data.Coerce
import           Data.Containers.ListUtils
import           Data.Default
import           Data.Either
import           Data.Flags
import           Data.Foldable
import           Data.Maybe
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Text.Encoding
import           Database.Persist
import qualified Database.Persist          as DB
import qualified Polysemy                  as P
import           Regex.RE2                 as RE2
import           TextShow

checkWatchWords :: (BotC r, P.Member Persistable r) => P.Sem r ()
checkWatchWords = void $ react @'MessageCreateEvt $ \msg -> handleFailByLogging $ do
  Just self <- getBotUser
  when (self ^. #id /= msg ^. #author) $ do
    Just g <- pure $ msg ^. #guildID
    wrds <- fmap (map DB.entityVal) $ db $ DB.selectList [WatchWordGuild DB.==. g] []
    let wrdMatches = filter (isJust . (flip RE2.find (encodeUtf8 $ msg ^. #content . strict) <=< forget . cmpl . encodeUtf8 . getText)) wrds
        usersToPing = coerce $ nubOrd $ map watchWordUser wrdMatches :: [Snowflake User]
    for_ usersToPing $ \usr -> do
      perms <- (coerce $ msg ^. #channelID :: Snowflake GuildChannel) `permissionsIn'` usr
      when (usr /= (msg ^. #author) && (perms `containsAll` viewChannel)) $ do
        Just auth <- upgrade $ msg ^. #author
        void $
          tell @T.Text usr $
            (auth ^. #username . strict)
            <> " mentioned a watch word in their message:\n> "
            <> T.take 300 (msg ^. #content . strict)
            <> "\n"
            <> messageUrl msg g
  where
    getText (WatchWord _ _ txt) = txt
    messageUrl :: Message -> Snowflake Guild -> T.Text
    messageUrl msg g = "<https://discord.com/channels/"
                      <> showt g <> "/"
                      <> showt (msg ^. #channelID) <> "/"
                      <> showt (msg ^. #id) <> "/>"
    forget (Left _)  = Nothing
    forget (Right x) = Just x

registerWatchWordCommands ::
  ( BotC r
  , P.Members
   '[ Persistable
    ] r
  ) => P.Sem (DSLState FullContext r) ()
registerWatchWordCommands =
  group "words" $ do
    void $
      command @'[[Text]] "add" $ \ctx wrds -> do
        Just gid <- pure $ getID <$> ctx ^. #guild
        let goodWrds = filter (isRight . cmpl . encodeUtf8) wrds
        res <- db $ traverse (insertUnique . WatchWord (getID $ ctx ^. #user) gid . T.map toLower) goodWrds
        void $
          tellt_ ctx $
            if length wrds /= length goodWrds
              then "One ot more patterns are invalid and therefore weren't added."
              else if any isNothing res
                then "Words added successfully (one or more already exist, however)"
                else "Words added successfully"

    void $
      command @'[[Text]] "remove" $ \ctx wrds -> do
        Just gid <- pure $ getID <$> ctx ^. #guild
        db $ traverse_ (DB.deleteBy . UserGuildText (getID $ ctx ^. #user) gid . T.map toLower) wrds
        void $
          tellt_ ctx "Words removed successfully"

    void $
      command @'[] "list" $ \ctx -> do
        Just gid <- pure $ getID <$> ctx ^. #guild
        let getWord (WatchWord _ _ wrd) = wrd
        wrds <- db $ map (getWord . entityVal) <$> selectList [WatchWordGuild ==. gid, WatchWordUser ==. getID (ctx ^. #user)] [Asc WatchWordText]
        void $
            if null wrds
              then tellt_ ctx "You have no watch words."
              else
                let opts = def
                        & #embed ?~
                          ( def
                          & #title ?~ "Your words are:"
                          & #description ?~ T.concat ((\w -> "• " <> w <> "\n") <$> wrds) ^. lazy
                          )
                in invoke_ $ CreateMessage (ctx ^. #channel) opts

    void $
      command @'[] "clear" $ \ctx -> do
        Just gid <- pure $ getID <$> ctx ^. #guild
        db $ deleteWhere [WatchWordGuild ==. gid, WatchWordUser ==. getID (ctx ^. #user)]
        void $
          tellt_ ctx "Words cleared successfully"

cmpl :: ByteString -> Either Error Pattern
cmpl = compileWith defaultOptions
  { optionCaseSensitive = False
  , optionWordBoundary = True
  , optionPosixSyntax  = True
  }
