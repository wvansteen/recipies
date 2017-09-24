{-# LANGUAGE OverloadedStrings #-}

module TestHelper where

import Control.Monad.Logger (runNoLoggingT)
import Database.Persist.Sql (runMigration, runSqlPool)
import Database.Persist.Sqlite (ConnectionPool, createSqlitePool)
import Network.HTTP.Client (newManager, defaultManagerSettings)
import Network.Wai.Handler.Warp (Port, testWithApplication)
import Servant.Client (ClientM, ServantError, BaseUrl (BaseUrl), ClientEnv (ClientEnv), runClientM)
import Servant.Common.BaseUrl (Scheme (Http))
import Test.Mockery.Directory (inTempDirectory)

import App (app)
import Models (migrateAll)

testConnectionPool :: IO ConnectionPool
testConnectionPool = inTempDirectory $ runNoLoggingT (createSqlitePool "sqlite.db" 5)

withApp ::
  (Port -> IO a)
  -> IO a
withApp action =
  let
    migratedApplication = do
      pool <- testConnectionPool
      runSqlPool (runMigration migrateAll) pool
      return $ app pool
  in
    testWithApplication migratedApplication action

executeRequest ::
  Port
  -> ClientM a
  -> IO (Either ServantError a)
executeRequest port request = do
  manager <- newManager defaultManagerSettings
  let baseUrl = BaseUrl Http "localhost" port ""
  let clientEnv = ClientEnv manager baseUrl
  runClientM request clientEnv
