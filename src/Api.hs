{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
module Api where

import Data.Int (Int64)
import Servant (Capture, Get, JSON, Post, Proxy, Proxy (Proxy), ReqBody, (:>), (:<|>))
import Models (Key, Recipe)

type RecipeApi =
    "recipe" :> ReqBody '[JSON] Recipe :> Post '[JSON] (Key Recipe)
  :<|> "recipe" :> Get '[JSON] [Recipe]
  :<|> "recipe" :> Capture "recipeKey" Int64 :> Get '[JSON] (Maybe Recipe)

api :: Proxy RecipeApi
api = Proxy
