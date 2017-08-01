{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Api where

import Data.Text
import Servant

import Models

type RecipeApi =
    "recipe" :> ReqBody '[JSON] Recipe :> Post '[JSON] (Key Recipe)
  :<|> "recipe" :> Get '[JSON] [Recipe]
  :<|> "recipe" :> Capture "recipeName" Text :> Get '[JSON] (Maybe Recipe)

api :: Proxy RecipeApi
api = Proxy
