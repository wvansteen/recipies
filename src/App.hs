{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module App where

import Control.Monad.IO.Class
import Control.Monad.Logger (runStderrLoggingT)
import Data.Text
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import Servant

import Api
import Models

server :: ConnectionPool -> Server RecipeApi
server pool = recipeAddH :<|> recipesGetH :<|> recipeGetH
  where
    recipeAddH newRecipe = liftIO $ recipeAdd newRecipe
    recipeGetH recipeId = liftIO $ recipeGet recipeId
    recipesGetH = liftIO recipesGet

    recipeAdd :: Recipe -> IO (Key Recipe)
    recipeAdd newRecipe = flip runSqlPersistMPool pool $ insert newRecipe

    recipeGet :: Text -> IO (Maybe Recipe)
    recipeGet name = flip runSqlPersistMPool pool $ do
      mRecipe <- selectFirst [RecipeName ==. name] []
      return $ entityVal <$> mRecipe

    recipesGet :: IO [Recipe]
    recipesGet = flip runSqlPersistMPool pool $ do
      recipes <- selectList [] []
      return $ entityVal <$> recipes

app :: ConnectionPool -> Application
app pool = serve api $ server pool

mkApp :: FilePath -> IO Application
mkApp sqliteFile = do
  pool <- runStderrLoggingT $ createSqlitePool (pack sqliteFile) 5
  runSqlPool (runMigration migrateAll) pool
  return $ app pool
