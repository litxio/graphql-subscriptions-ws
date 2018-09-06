
module GraphQL.Subscriptions.Transport.WS.Server where

import           GraphQL.API (HasObjectDefinition)
import           GraphQL.Resolver (HasResolver)
import           Control.Monad      (forever)
import           Control.Exception (finally)
import qualified Data.Text          as T
import qualified Network.WebSockets as WS
import Control.Concurrent.MVar
import Control.Monad.Loops (iterateM_)
import Data.Aeson

import GraphQL.Subscriptions.Transport.WS.Internal
import GraphQL.Subscriptions.Transport.WS.ServerOptions
import GraphQL.Subscriptions.Transport.WS.MessageLoop

wsServer :: (HasResolver m api, HasObjectDefinition api) => ServerOptions m api -> WS.ServerApp
wsServer server pending = do
  conn <- WS.acceptRequestWith pending $
    WS.defaultAcceptRequest { WS.acceptSubprotocol = Just "graphql-ws" }
  WS.forkPingThread conn 30
  finally (talk conn server) disconnect

disconnect :: IO ()
-- TODO
disconnect = return ()


talk :: (HasResolver m api, HasObjectDefinition api) => WS.Connection -> ServerOptions m api -> IO ()
talk conn server = do
  initialCtx <- newConnectionContext conn
  iterateM_ loop initialCtx
  where
    loop ctx = do
      putStrLn "Awaiting message..."
      msg <- eitherDecode <$> WS.receiveData conn :: IO (Either String OperationMessage)
      case msg of
        Right m -> onMessage server ctx m
        Left e  -> putStrLn $ "Bad message: " ++ e
      return ctx

