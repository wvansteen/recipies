{-# LANGUAGE OverloadedStrings #-}

module AppSpec where

import Data.ByteString.Lazy.Char8 (pack)
import Data.Int (Int64)
import Database.Persist.Sql (toSqlKey)
import Network.HTTP.Types (status404)
import Servant ((:<|>) ((:<|>)))
import Servant.Client (ClientM, ServantError (FailureResponse), client)
import Test.Hspec (Spec, around, describe, it, shouldBe, shouldReturn)
import Test.HUnit.Base (assertFailure)

import Api (api)
import Models (Key, Recipe (Recipe))
import TestHelper (executeRequest, withApp)

addRecipe :: Recipe -> ClientM (Key Recipe)
getRecipes :: ClientM [Recipe]
getRecipe :: Int64 -> ClientM Recipe
(addRecipe :<|> getRecipes :<|> getRecipe) = client api

spec :: Spec
spec =
  around withApp $
    describe "/recipe" $ do
      it "Adds a recipe and returns the id" $ \ port -> do
        let
          recipe1 = Recipe "1" "1"
          recipe2 = Recipe "2" "2"
        executeRequest port (addRecipe recipe1) `shouldReturn` Right (toSqlKey 1)
        executeRequest port (addRecipe recipe2) `shouldReturn` Right (toSqlKey 2)

      it "Lists all recipes" $ \ port -> do
        let
          recipe1 = Recipe "1" "1"
          recipe2 = Recipe "2" "2"
        _ <- executeRequest port (addRecipe recipe1)
        _ <- executeRequest port (addRecipe recipe2)
        executeRequest port getRecipes `shouldReturn` Right [recipe1, recipe2]

      it "Returns the recipe with the passed id" $ \ port -> do
        let recipe1 = Recipe "1" "1"
        _ <- executeRequest port (addRecipe recipe1)
        executeRequest port (getRecipe 1) `shouldReturn` Right (Recipe "1" "1")

      it "Returns a 404 when the recipe doesn't exist" $ \ port -> do
        response <- executeRequest port (getRecipe 42)
        case response of
          Left (FailureResponse _ status _ body) -> do
            status `shouldBe` status404
            body `shouldBe` pack ""
          _ -> assertFailure "Did not return a FailureResponse"
