module Main where

import Control.Monad.Logger (runStderrLoggingT)
import Data.Text (pack)
import Database.Persist.Sql (runMigration, runSqlPool)
import Database.Persist.Sqlite (createSqlitePool)
import Network.Wai.Handler.Warp (run)
import Servant.Server (Application)

import App (app)
import Models (migrateAll)

main :: IO()
main = run 3000 =<< mkApp "sqlite.db"

mkApp :: FilePath -> IO Application
mkApp sqliteFile = do
  pool <- runStderrLoggingT $ createSqlitePool (pack sqliteFile) 5
  runSqlPool (runMigration migrateAll) pool
  return $ app pool
