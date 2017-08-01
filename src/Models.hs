{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}


module Models where

import Data.Aeson
import Data.Text
import Database.Persist.TH
import GHC.Generics

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
  Recipe
    name Text
    UniqueRecipeName name
    description Text
    deriving Eq Show Generic
|]

instance FromJSON Recipe where
  parseJSON = withObject "Recipe" $ \v ->
    Recipe <$> v .: "name" <*> v  .: "description"

instance ToJSON Recipe where
  toJSON (Recipe name description) = object [
    "name" .= name
    ,"description" .= description]
