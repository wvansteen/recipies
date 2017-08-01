module TestHelper where

import Control.Exception (throwIO)
import Control.Monad.Trans.Except
import Network.HTTP.Client (Manager, newManager, defaultManagerSettings)
import Network.HTTP.Types
import Network.Wai (Application)
import Network.Wai.Handler.Warp
import Servant.Client
import Test.Hspec

withClient :: IO Application -> SpecWith Host -> SpecWith ()
withClient app innerSpec =
  beforeAll (newManager defaultManagerSettings) $
    flip aroundWith innerSpec $ \ action manager ->
      testWithApplication app $ \ port ->
        action (manager, BaseUrl Http "localhost" port "")

type Host = (Manager, BaseUrl)

try :: Host -> (Manager -> BaseUrl -> ClientM a) -> IO a
try (manager, baseUrl) action = either throwIO return =<<
  runExceptT (action manager baseUrl)

shouldThrowErrorStatus :: IO a -> Status -> Expectation
shouldThrowErrorStatus action status = action `shouldThrow` ((==) status . responseStatus)
