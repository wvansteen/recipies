module AppSpec where

import Network.HTTP.Client (Manager)
import Network.HTTP.Types
import Servant
import Servant.Client
import Test.Hspec

import Api
import App hiding (getRecipes)
import Models
import TestHelper

getRecipes :: Manager -> BaseUrl -> ClientM [Recipe]
getRecipe :: Integer -> Manager -> BaseUrl -> ClientM Recipe
getRecipes :<|> getRecipe = client api

spec :: Spec
spec =
  describe "/item" $
    withClient mkApp $ do
      it "lists an example item" $ \ host ->
        try host getRecipes `shouldReturn` [Recipe 0 "example recipe"]
      it "allows to show items by id" $ \ host ->
        try host (getRecipe 0) `shouldReturn` Recipe 0 "example recipe"
      it "throws a 404 for missing items" $ \ host ->
        try host (getRecipe 42) `shouldThrowErrorStatus` notFound404
