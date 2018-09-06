
module GraphQL.Subscriptions.Transport.WS.Internal where

import GraphQL
import qualified GraphQL.Internal.Syntax.AST as AST
import qualified Network.WebSockets as WS
import qualified Data.Aeson as Aeson
import Data.Aeson ((.=), (.:), (.:?))
import Data.Maybe
import qualified Data.HashMap.Lazy as HML
import GHC.Generics
import Control.Monad (forever, when)
import Control.Exception (throw)
import Control.Concurrent.Chan.Unagi (InChan, OutChan, newChan, readChan)
import Control.Concurrent.Async (Async, async, cancel)
import qualified STMContainers.Map as SC
import Data.Text (Text)
import GraphQL.Subscriptions.Transport.WS.MessageType
import GraphQL.Subscriptions.Transport.WS.SourceEvent
import GraphQL.Subscriptions.Transport.WS.Errors


data ActiveSubscription = ActiveSubscription {
  sourceStreamIn :: InChan SourceEvent,
  sourceStreamOut :: OutChan SourceEvent,

  responseStreamIn :: InChan Response,
  responseStreamOut :: OutChan Response,

  createSourceThread :: Maybe (Async ()),  -- Nothing if not yet initialized
  createResponseThread :: Maybe (Async ()), -- Nothing if not yet initialized
  sendResponseThread :: Async ()
 }

newSubscription :: ConnectionContext -> Text -> IO ActiveSubscription
newSubscription connCtx opId = do
  (sourceStreamIn, sourceStreamOut) <- newChan
  (responseStreamIn, responseStreamOut) <- newChan
  responseWatcher <- async $ forever $ do
    result <- readChan responseStreamOut
    putStrLn $ "Read " ++ show result ++ " from outChan"
    sendMessage connCtx (Just opId) GQL_DATA (Just $ Aeson.toJSON result)
  return $! ActiveSubscription sourceStreamIn sourceStreamOut
                               responseStreamIn responseStreamOut
                               Nothing Nothing responseWatcher

cancelSubscription :: ActiveSubscription -> IO ()
cancelSubscription ActiveSubscription{..} = do
  maybeCancelThread createSourceThread
  maybeCancelThread createResponseThread
  maybeCancelThread $ Just sendResponseThread
    where 
      maybeCancelThread (Just t) = cancel t
      maybeCancelThread Nothing = return ()


type OperationMap = SC.Map Text ActiveSubscription

data ConnectionContext = ConnectionContext {
  -- initPromise?: Promise<any>,
  -- isLegacy: boolean,
  socket :: WS.Connection,
  -- request: IncomingMessage,
  operations :: OperationMap -- { [opId: string]: ExecutionIterator }
}

newConnectionContext conn = ConnectionContext conn <$> SC.newIO

data ExecutionParams = ExecutionParams {
  epQuery :: Text, --  | DocumentNode;
  epVariables :: GraphQL.VariableValues,
  epOperationName :: Maybe Text,
  -- epContext :: tContext,
  -- formatResponse?: Function,
  -- formatError?: Function,
  -- callback?: Function,
  epSchema :: Maybe GraphQL.Schema
}

data OperationMessage = OperationMessage {
  omPayload :: Maybe Aeson.Value,
  omId :: Maybe Text,
  omType :: Text
} deriving (Eq, Generic, Show)

instance Aeson.ToJSON OperationMessage where
  toJSON OperationMessage{..} = Aeson.object [ "payload" .= Aeson.toJSON omPayload,
                                               "id"      .= Aeson.toJSON omId,
                                               "type"    .= Aeson.toJSON omType ]
    
instance Aeson.FromJSON OperationMessage where
  parseJSON = Aeson.withObject "OperationMessage" $ \v ->
    OperationMessage <$> v .:? "payload"
                     <*> v .:? "id"
                     <*> v .: "type"



sendMessage :: ConnectionContext -> Maybe Text -> MessageType -> Maybe Aeson.Value -> IO ()
sendMessage connCtx opId type_ payload =
  let message = OperationMessage payload opId (msgTypeToText type_)
   in do
     putStrLn $ "Sending message: " ++ show message
     WS.sendTextData (socket connCtx) $ Aeson.encode message

sendKeepAlive :: ConnectionContext -> Int -> IO ()
sendKeepAlive connCtx mills = sendMessage connCtx Nothing GQL_CONNECTION_KEEP_ALIVE Nothing

sendError :: ConnectionContext
          -> Maybe Text
          -> Maybe Aeson.Value
          -> Maybe MessageType
          -> IO ()
sendError connCtx opId errorPayload overrideDefaultErrorType = do
  let sanitizedOverrideDefaultErrorType = fromMaybe GQL_ERROR overrideDefaultErrorType
  when (sanitizedOverrideDefaultErrorType /= GQL_ERROR
        && sanitizedOverrideDefaultErrorType /= GQL_CONNECTION_ERROR)
       $ throw $ BadClientRequest "overrideDefaultErrorType should be one of the \
                                   \ allowed error messages: GQL_CONNECTION_ERROR or GQL_ERROR'"
  sendMessage connCtx opId sanitizedOverrideDefaultErrorType errorPayload

sendErrorText :: ConnectionContext
          -> Maybe Text
          -> Text
          -> Maybe MessageType
          -> IO ()
sendErrorText connCtx opId msg overrideDefaultErrorType =
  sendError connCtx opId (Just $ errorMsgPayload msg) overrideDefaultErrorType
    where 
      errorMsgPayload :: Text -> Aeson.Value
      errorMsgPayload msg = Aeson.Object $ HML.fromList [("message", Aeson.String msg)]
