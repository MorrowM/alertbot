{-# LANGUAGE StandaloneDeriving, DerivingVia #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Discord.Alertbot.Orphans where

import Database.Persist
import Database.Persist.Sql
import Calamity
import Data.Word

deriving via Word64 instance PersistField (Snowflake a)
deriving via Word64 instance PersistFieldSql (Snowflake a)