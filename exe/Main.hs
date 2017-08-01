module Main where

import Network.Wai.Handler.Warp

import App

main :: IO()
main = run 3000 =<< mkApp "sqlite.db"
