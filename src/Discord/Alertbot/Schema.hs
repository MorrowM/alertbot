{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Discord.Alertbot.Schema where

import Data.Text (Text)
import Database.Persist.TH
import Calamity
import Discord.Alertbot.Orphans ()

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
    message (Snowflake Message)

    UserGuildMessage user message
  
  ReactionGuildWatch
    user (Snowflake User)
      guild (Snowflake Guild)

    UserGuild user guild
|]