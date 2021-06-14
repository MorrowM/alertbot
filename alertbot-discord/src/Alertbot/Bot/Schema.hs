{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Alertbot.Bot.Schema where

import           Alertbot.Bot.Orphans ()
import           Calamity
import           Data.Text            (Text)
import           Database.Persist.TH

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
  WatchWord
    user (Snowflake User)
    guild (Snowflake Guild)
    text Text

    UserGuildText user guild text

  ReactionMessageWatch
    user (Snowflake User)
    guild (Snowflake Guild)
    channel (Snowflake TextChannel)
    message (Snowflake Message)

    UserGuildMessage user channel message

  ReactionGuildWatch
    user (Snowflake User)
    guild (Snowflake Guild)

    UserGuild user guild
|]
