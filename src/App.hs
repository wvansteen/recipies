{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module App where

import Control.Monad.IO.Class (liftIO)
import Database.Persist (entityVal, get, insert, selectList)
import Database.Persist.Sql (ConnectionPool, runSqlPersistMPool, toSqlKey)
import Servant ((:<|>) ((:<|>)))
import Servant.Server (Application, Server, serve)

import Api (api, RecipeApi)

server :: ConnectionPool -> Server RecipeApi
server pool = recipeAdd :<|> lookupRecipes :<|> lookupRecipe
  where
    recipeAdd newRecipe = let
      statement = insert newRecipe
      in liftIO $ runSqlPersistMPool statement pool

    lookupRecipe recipeId = let
      statement = get (toSqlKey recipeId)
      in liftIO $ runSqlPersistMPool statement pool

    lookupRecipes = let
      query = do
        recipes <- selectList [] []
        return $ entityVal <$> recipes
      in liftIO $ runSqlPersistMPool query pool

app :: ConnectionPool -> Application
app connectionPool = serve api $ server connectionPool
