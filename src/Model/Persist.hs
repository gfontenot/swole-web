{-# LANGUAGE FlexibleInstances #-}

module Model.Persist where

import ClassyPrelude.Yesod
import Database.Persist.Quasi

import Model.Weight

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share
    [ mkPersist sqlSettings
    , mkMigrate "migrateAll"
    ]
    $(persistFileWith lowerCaseSettings "config/models")
