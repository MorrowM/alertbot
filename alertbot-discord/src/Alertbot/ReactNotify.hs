module Alertbot.ReactNotify where

import           Alertbot.Bot.Config
import           Alertbot.Bot.Database
import           Alertbot.Bot.Schema
import           Alertbot.Bot.Util
import           Calamity
import           Calamity.Cache.Eff        (getBotUser)
import           Calamity.Commands
import           Calamity.Commands.Context (FullContext)
import           Control.Lens
import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Lens           as A (_String, key)
import           Data.Foldable
import           Data.Int
import           Data.List                 hiding (group)
import qualified Data.Text.Encoding        as BS
import qualified Data.Text.Lazy            as L
import qualified Data.Text.Lazy.Builder    as LB
import           Data.Text.Lens
import qualified Database.Persist          as P
import           Network.HTTP.Req
import qualified Polysemy                  as P
import qualified Polysemy.Fail             as P
import qualified Polysemy.Reader           as P
import           Text.Read
import           TextShow

registerReactNotifyCommands ::
  ( BotC r
  , P.Members
   '[ Persistable
    ] r
  ) => P.Sem (DSLState FullContext r) ()
registerReactNotifyCommands =
  requires [inGuild] $ group "reactnotify" $ do
    group "all" $ do
      void $ command @'[] "enable" $ \ctx -> do
        Just g <- pure $ ctx ^. #guild
        res <- db $ P.insertBy $ ReactionGuildWatch (ctx ^. #user . #id) (g ^. #id)
        void $ case res of
          Left _ -> tellt_ ctx "You are already subscribed to reactions in this server."
          Right _ -> tellt_ ctx "Successfully subscribed to reactions in this server."

      void $ command @'[] "disable" $ \ctx -> do
        Just g <- pure $ ctx ^. #guild
        db $ P.deleteWhere
          [ReactionGuildWatchUser P.==. ctx ^. #user . #id
          , ReactionGuildWatchGuild P.==. g ^. #id
          ]
        void $ tellt_ ctx "Successfully unsubscribed from reactions in this server."

    void $ command @'[] "info" $ \ctx -> do
      Just g <- pure $ ctx ^. #guild
      mGuildSub <- db $ P.getBy (UserGuild (ctx ^. #user . #id) (g ^. #id))
      void $ case mGuildSub of
        Nothing -> tellt_ ctx "Serverwide reactions are turned off."
        Just _  -> tellt_ ctx "Serverwide reactions are turned on."
      msgs <- fmap (map P.entityVal) $ db $ P.selectList
        [ ReactionMessageWatchUser P.==. ctx ^. #user . #id
        , ReactionMessageWatchGuild P.==. g ^. #id
        ] []
      let txt = if null msgs
                  then "" :: LB.Builder
                  else "You are subscribed to the following messages:\n"
          txt' = txt <> mconcat
            ["<https://discord.com/channels/"
            <> fromText (showt gid) <> "/"
            <> fromText (showt cid) <> "/"
            <> fromText (showt mid) <> "/>"
            | (ReactionMessageWatch _ gid cid mid) <- msgs
            ]
      void $ tell ctx (toLazyText txt')

    group "msg" $ do
      void $ command @'[Snowflake Message, GuildChannel] "add" $ \ctx msg gchan -> do
        case gchan of
          GuildTextChannel chan -> do
            existing <- db $ P.count [ReactionMessageWatchUser P.==. ctx ^. #user . #id]
            if existing > 30
              then void $ tellt_ ctx "Sorry, you cannot subscribe to any more messages."
              else do
                res <- db $ P.insertBy $ ReactionMessageWatch (ctx ^. #user . #id) (chan ^. #guildID) (chan ^. #id) msg
                void $ case res of
                  Left _ -> tellt_ ctx "You are already subscribed to this message."
                  Right _ -> tellt_ ctx "Successfully subscribed to the message."
          _ -> void $ tellt_ ctx "The channel must be a text channel."

      void $ command @'[Snowflake Message, GuildChannel] "remove" $ \ctx msg gchan -> do
        case gchan of
          GuildTextChannel chan -> do
            db $ P.deleteWhere
              [ ReactionMessageWatchUser P.==. (ctx ^. #user . #id)
              , ReactionMessageWatchGuild P.==. (chan ^. #guildID)
              , ReactionMessageWatchChannel P.==. (chan ^. #id)
              , ReactionMessageWatchMessage P.==. msg
              ]
            void $ tellt_ ctx "Successfully unsubscribed from the message."
          _ -> void $ tellt_ ctx "The channel must be a text channel."
      void $ command @'[Snowflake Message, GuildChannel] "remove" $ \ctx msg gchan -> do
        case gchan of
          GuildTextChannel chan -> do
            db $ P.deleteWhere
              [ ReactionMessageWatchUser P.==. (ctx ^. #user . #id)
              , ReactionMessageWatchGuild P.==. (chan ^. #guildID)
              , ReactionMessageWatchChannel P.==. (chan ^. #id)
              , ReactionMessageWatchMessage P.==. msg
              ]
            void $ tellt_ ctx "Successfully unsubscribed from the message."
          _ -> void $ tellt_ ctx "The channel must be a text channel."

      void $ command @'[] "clear" $ \ctx -> do
        Just g <- pure $ ctx ^. #guild
        db $ P.deleteWhere
          [ ReactionMessageWatchUser P.==. (ctx ^. #user . #id)
          , ReactionMessageWatchGuild P.==. (g ^. #id)
          ]
        void $ tellt_ ctx "Successfully unsubscribed from all messages in the server."

reactionHandler :: (BotC r, P.Member Persistable r, P.Member (P.Reader Config) r) => P.Sem r ()
reactionHandler = void . P.runFail . react @'RawMessageReactionAddEvt $ \rct -> do
  Just self <- getBotUser
  when (self ^. #id /= rct ^. #userID) $ do
    Just gid <- pure $ rct ^. #guildID
    Right mesg <- invoke $ GetMessage rct rct
    uid <- case mesg ^. #webhookID of
      Nothing -> pure $ mesg ^. #author
      Just _ -> do
        pk <- pluralkit (mesg ^. #id)
        pure $ case pk of
          Nothing     -> mesg ^. #author
          Just trueID -> trueID
    serverwide <- fmap (map $ reactionGuildWatchUser . P.entityVal) $ db $ P.selectList
      [ ReactionGuildWatchGuild P.==. gid
      , ReactionGuildWatchUser P.==. uid
      ] []
    thismsg <- fmap (map $ reactionMessageWatchUser . P.entityVal) $ db $ P.selectList
      [ ReactionMessageWatchMessage P.==. rct ^. #messageID
      , ReactionMessageWatchChannel P.==. coerceSnowflake @Channel @TextChannel (rct ^. #channelID)
      ] []

    let allSubs = serverwide `union` thismsg

    Just usr <- upgrade (rct ^. #userID)
    info $ "Notifying " <> showt allSubs
    for_ allSubs $ \uid -> do
      telllt_ uid $ LB.toLazyText $ mconcat
        [ fromLazyText (usr ^. #username)
        , " reacted to a subscribed message with "
        , fromLazyText $ renderEmoji $ rct ^. #emoji
        , "\n"
        , fromLazyText $ quoteLines $ clip 300 (mesg ^. #content)
        , "\n"
        , messageUrl gid mesg mesg
        ]


messageUrl :: (HasID Guild g, HasID Channel c, HasID Message m) => g -> c -> m -> LB.Builder
messageUrl gid cid mid = "<https://discord.com/channels/"
            <> fromText (showt $ getID @Guild gid) <> "/"
            <> fromText (showt $ getID @Channel cid) <> "/"
            <> fromText (showt $ getID @Message mid) <> "/>"

renderEmoji :: RawEmoji -> L.Text
renderEmoji (UnicodeEmoji txt) = txt
renderEmoji (CustomEmoji pemj) = "<:" <> pemj ^. #name <> ":" <> pemj ^. #id . to showtl <> ">"

clip :: Int64 -> L.Text -> L.Text
clip n txt =
  if L.length txt <= n
    then txt
    else L.take n txt `L.append` "..."

pluralkit :: (P.Member (P.Embed IO) r, P.Member (P.Reader Config) r) => Snowflake Message -> P.Sem r (Maybe (Snowflake User))
pluralkit mid = do
  token <- P.asks @Config $ view #pkToken
  P.embed @IO $ runReq defaultHttpConfig $ do
    res <- req
      GET
      (https "api.pluralkit.me" /: "v1" /: "msg" /: showt mid)
      NoReqBody
      (jsonResponse @Value)
      (header "Authorization" (BS.encodeUtf8 token))

    case responseStatusCode res of
      200 -> do
        let uidTxt = responseBody res ^? A.key "sender" . _String . unpacked
            uid = fmap Snowflake . readMaybe =<< uidTxt
        pure uid
      _ -> pure Nothing

{-
pluralkitIO :: _ -> Snowflake Message -> IO (Maybe (Snowflake User))
pluralkitIO txt mid = P.runFinal @IO
  . P.embedToFinal @IO .
   P.runReader (Config { pkToken = txt })
   $ pluralkit mid
-}

quoteLines :: L.Text -> L.Text
quoteLines = quote . L.replace "\n" "\n> "
