{-# LANGUAGE OverloadedStrings #-}

module AppSpec where

import Data.ByteString.Lazy.Char8 (pack)
import Data.Int (Int64)
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
getRecipe :: Int64 -> ClientM (Maybe Recipe)
(addRecipe :<|> getRecipes :<|> getRecipe) = client api

spec :: Spec
spec =
  around withApp $
    describe "/recipe" $ do
      it "Lists all recipes" $ \ port ->
        executeRequest port getRecipes `shouldReturn` Right [Recipe "" "example recipe"]
      it "Returns the recipe with the passed id" $ \ port ->
        executeRequest port (getRecipe 0) `shouldReturn` Right (Just (Recipe "" "example recipe"))
      it "Returns a 404 when the recipe doesn't exist" $ \ port -> do
        response <- executeRequest port (getRecipe 42)
        case response of
          Left (FailureResponse _ status _ body) -> do
            status `shouldBe` status404
            body `shouldBe` pack ""
          _ -> assertFailure "Did not return a FailureResponse"
