{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE NamedFieldPuns #-}

module GraphQL.Subscriptions.Transport.WS.MessageLoop where

-- | This module is a loose translation of the main message loop from 
-- subscriptions-transport-ws 
-- (https://github.com/apollographql/subscriptions-transport-ws/blob/master/src/server.ts)

import qualified Data.Aeson as Aeson
import qualified Data.Map as Map
import Data.Aeson ((.=), (.:), (.:?))
import GraphQL
import GraphQL.API as GAPI
import GraphQL.Value
import GraphQL.Resolver
import GraphQL.Internal.Execution
import GraphQL.Internal.Name
import GraphQL.Internal.Validation
import qualified GraphQL.Internal.Syntax.Parser as Parser
import qualified GraphQL.Internal.Syntax.AST as AST
import Data.Attoparsec.Text (parseOnly, endOfInput)
import Control.Concurrent.Chan.Unagi
import Control.Concurrent hiding (newChan, Chan, writeChan, readChan)
import Control.Monad (when, forever, unless, (>=>))
import Control.Exception hiding (Handler)
import Control.Concurrent.Async (async, Async, cancel)
import Data.Text (Text, pack)
import Data.Proxy (Proxy(..))
import Data.Bifunctor (first)
import Data.Semigroup (sconcat)
import Data.Coerce (coerce, Coercible)
import Control.Concurrent.STM (atomically)
import Data.Maybe
import qualified Network.WebSockets as WS
import qualified STMContainers.Map as SC
import qualified Data.HashMap.Strict as HM
import qualified Data.HashMap.Lazy as HML
import Lens.Micro
import Lens.Micro.Aeson

import GraphQL.Subscriptions.Transport.WS.MessageType
import GraphQL.Subscriptions.Transport.WS.ServerOptions
import GraphQL.Subscriptions.Transport.WS.Internal
import GraphQL.Subscriptions.Transport.WS.Errors



-- Based on https://github.com/graphql/graphql-js/blob/master/src/utilities/getOperationAST.js
getOperationAST :: AST.QueryDocument -> Maybe Text -> Maybe AST.OperationDefinition
getOperationAST (AST.QueryDocument defs) opName = 
  if length ops == 1
     then Just $ head ops
     else findMatchingDef ops
  where 
    ops = catMaybes $ unOp <$> defs
    unOp (AST.DefinitionOperation op) = Just op
    unOp _                       = Nothing
    findMatchingDef opz = listToMaybe $ filter nameMatches opz
    nameMatches (AST.Query (AST.Node n _ _ _)) = (unName <$> n) == opName
    nameMatches (AST.Mutation (AST.Node n _ _ _)) = (unName <$> n) == opName
    -- nameMatches (DefinitionOperation (Subscription (Node n _ _ _))) = n == opName
    nameMatches (AST.AnonymousQuery _) = False

firstOperationName :: AST.OperationDefinition -> Maybe Text
firstOperationName (AST.Query (AST.Node n _ _ ss)) = Just $ selectionSetName ss
firstOperationName (AST.Mutation (AST.Node n _ _ ss)) = Just $ selectionSetName ss
firstOperationName (AST.Subscription (AST.Node n _ _ ss)) = Just $ selectionSetName ss
firstOperationName (AST.AnonymousQuery _) = Nothing

selectionSetName (AST.SelectionField (AST.Field _ name _ _ _) : _) = unName name
-- This should be unreachable?
selectionSetName _ = error "Panic! Bad selection set"

-- | Parse a query document.
parseQuery :: Text -> Either Text AST.QueryDocument
parseQuery query = first pack (parseOnly (Parser.queryDocument <* endOfInput) query)

unsubscribe :: ConnectionContext -> Text -> IO ()
unsubscribe connectionContext opId = do
  mbSubs <- atomically $ do
    mbSubs <- SC.lookup opId $ operations connectionContext 
    SC.delete opId $ operations connectionContext
    return mbSubs

  case mbSubs of 
    Just subs -> cancelSubscription subs
    Nothing   -> return ()
  -- TODO
  -- if (this.onOperationComplete) {
  --   this.onOperationComplete(connectionContext.socket, opId);
  -- }


onMessage :: forall (api :: *) m. (HasResolver m api, HasObjectDefinition api) =>
  ServerOptions m api -> ConnectionContext -> OperationMessage -> IO ()
onMessage serverOpts connCtx message = do
  putStrLn $ "Got " ++ show message
  case parseMsgType (omType message) of 
    Just GQL_CONNECTION_INIT -> handleInit serverOpts connCtx message

    Just GQL_CONNECTION_TERMINATE -> putStrLn "Received CONNECTION_TERMINATE"
      -- connectionContext.socket.close(); Seems the library handles this for us?
      --
    Just GQL_START -> handleStart serverOpts connCtx message

    Just GQL_STOP  -> handleStop serverOpts connCtx message

    Nothing -> throw $ BadClientRequest ("Unknown message type: " <> omType message)

    otherwise -> sendErrorText connCtx Nothing "Invalid message type!" Nothing
      
handleInit :: forall (api :: *) m. (HasResolver m api, HasObjectDefinition api) =>
  ServerOptions m api -> ConnectionContext -> OperationMessage -> IO ()
handleInit ServerOptions{..} connCtx message = handle (handleError Nothing) $ do
  connRes <- fromMaybe (return True) $ onConnect <*> Just (omPayload message) <*> Just connCtx
  unless connRes $ throw (BadClientRequest "Prohibited connection!")
  sendMessage connCtx Nothing GQL_CONNECTION_ACK Nothing
  case keepAlive of
    Nothing     -> return ()
    Just mills  -> do
      sendKeepAlive connCtx mills
      -- TODO: Keep running the Keepalive timer!
      error "keepAlive not really implemented..."
  where
    handleError :: Maybe Text -> SomeException -> IO ()
    handleError mbOpId e = sendErrorText connCtx mbOpId (pack $ show e) (Just GQL_CONNECTION_ERROR)
                    -- Am I supposed to close the connection here..?

handleStart :: forall (api :: *) m. (HasResolver m api, HasObjectDefinition api) =>
  ServerOptions m api -> ConnectionContext -> OperationMessage -> IO ()
handleStart serverOpts@ServerOptions{..} connCtx message = do
  when (isNothing $ omPayload message ^? _Just . ix "query")
       $ throw (BadClientRequest "No query given in GQL_START")
  let baseParamsOrEx = ExecutionParams 
        <$> mbToEither  (omPayload message ^? _Just . ix "query" . _String)   -- epQuery
                        "No query given in GQL_START"
        <*> toVarVals (omPayload message ^? _Just . ix "variables")           -- epVariables
        <*> Right (omPayload message ^? _Just . ix "operationName" . _String) -- epOperationName
        -- <*> Right Nothing                                                  -- epContext
        <*> Right (Just schema)                                               -- epSchema
  -- atomically $ SC.insert Chansly.empty opId (operations connCtx)
  params <- case baseParamsOrEx of
                  Left ex -> throw $ BadClientRequest ex
                  Right bp -> return bp

  -- TODO handle onOperation, which may overwrite params
  tSchema <- case epSchema params of 
               Nothing -> throw (ServerConfigException
                                    "Missing schema information. The GraphQL schema should\
                                    \ be provided either statically in `ServerOptions` or\
                                    \ as a property on the object returned from onOperation!")
               Just s  -> return s
  docAST <- case parseQuery (epQuery params) of
              Left msg -> throw (BadClientRequest msg)
              Right d  -> return d
  -- validDoc <- case compileQuery tSchema (epQuery params) of
  --               Left queryErr       -> throw (BadClientRequest $ formatError queryErr)
  --               Right qd            -> return qd
  opId <- case omId message of 
            Just id -> return id
            Nothing -> do
              sendErrorText connCtx Nothing "No opId given in GQL_START" Nothing
              throw (BadClientRequest "Received GQL_START with no operation ID")
  let opAST = getOperationAST docAST (Just opId)

  if isSubscription opAST
     then handleSubscription serverOpts connCtx opAST opId params
     else handleQueryOrMutation serverOpts connCtx opId tSchema docAST params

  where
    isSubscription (Just (AST.Subscription _)) = True
    isSubscription _ = False

-- buildSourceStream :: 

-- | Handle a new subscription request from the client by doing the following:
--
-- * Create a channel (concurrent queue -- we use unagi-chan's types) through which
--   source events will be communicated
-- * Kick off a thread that will provide source events, using the function provided in
--   sourceStreams (part of ServerOptions)
-- * Kick off another thread that waits for source events and, for each source event,
--   executes the GraphQL operation with the name of the requested subscription. This
--   operation must take a single argument named "sourceEvent"
--
-- The above mentioned mechanism -- passing the source event as a field argument -- is
-- a deviation from the spec, which instead stipulates that the event be passed in the
-- initialValue argument to the resolver.  graphql-api has a different way of doing
-- things so we adapt.
handleSubscription :: forall (api :: *) m. (HasResolver m api, HasObjectDefinition api) =>
  ServerOptions m api
  -> ConnectionContext 
  -> Maybe AST.OperationDefinition 
  -> Text 
  -> ExecutionParams 
  -> IO ()
handleSubscription ServerOptions{..} connCtx opAST opId params = do
   case opAST >>= firstOperationName >>= (`HM.lookup` sourceEventGenerators) of
     Nothing           -> throw $ ServerConfigException "No source stream for subscription"
     Just genSourceEvents -> do
       -- Create the channels and kick off the thread that waits for and sends execution results
       subs <- newSubscription connCtx opId
       -- Write to source stream
       createSourceThread <- async $ genSourceEvents (sourceStreamIn subs)
       -- Read from source stream, write to response stream
       createResponseThread <- async $ readChanForever (runSubscriptionQuery opId (fromJust opAST) (epVariables params)
                                                        >=> writeChan (responseStreamIn subs))
                                                       (sourceStreamOut subs)
       atomically $ SC.insert subs { createSourceThread = Just createSourceThread,
                                     createResponseThread = Just createResponseThread }
                              opId (operations connCtx)
  where 
    runSubscriptionQuery :: Text -> AST.OperationDefinition -> VariableValues -> AST.Value -> IO Response
    runSubscriptionQuery opId (AST.Subscription (AST.Node mbName varDefs directives [AST.SelectionField field]))
               variables val = do
      let docAST = AST.QueryDocument [AST.DefinitionOperation queryOp]
          queryOp = AST.Query (AST.Node mbName varDefs directives 
                                    [AST.SelectionField $ addParam field val])
          addParam :: AST.Field -> AST.Value -> AST.Field
          addParam (AST.Field mbAlias name [] directives ss) val
            = AST.Field mbAlias name [AST.Argument (Name "sourceEvent") val] directives ss

      document <- case validate schema docAST of 
                    Left validationErrs -> throw (BadClientRequest $ sconcat (formatError <$> validationErrs))
                    Right qd            -> return qd
      -- putStrLn $ "mkSelector: Going to execute document " ++ show document
      execute rootHandler document mbName variables
    runSubscriptionQuery _ _ _ _ = error "Panic! runSubscriptionQuery expects a subscription operation with\
                                          \ a single SelectionField"

    readChanForever :: (a -> IO ()) -> OutChan a -> IO ()
    readChanForever f outChan = forever $ do
      val <- readChan outChan
      f val

handleQueryOrMutation :: forall (api :: *) m. (HasResolver m api, HasObjectDefinition api) =>
  ServerOptions m api 
  -> ConnectionContext 
  -> Text 
  -> Schema 
  -> AST.QueryDocument 
  -> ExecutionParams
  -> IO ()
handleQueryOrMutation ServerOptions{..} connCtx opId tSchema docAST params = do
   validDoc <- case validate tSchema docAST of 
                 Left validationErrs -> throw (BadClientRequest $ sconcat (formatError <$> validationErrs))
                 Right qd            -> return qd
   response <- execute rootHandler validDoc
                       (Name <$> epOperationName params)
                       (epVariables params)
   case response of
     Success obj             -> do
       sendMessage connCtx (Just opId) GQL_DATA (Just $ Aeson.object ["data" .= obj])
       sendMessage connCtx (Just opId) GQL_COMPLETE Nothing
     PartialSuccess obj errs -> do
       sendMessage connCtx (Just opId) GQL_DATA (Just $ Aeson.object ["data" .= obj])
       sendMessage connCtx (Just opId) GQL_COMPLETE Nothing
     err                     ->
       sendErrorText connCtx Nothing (pack $ show err) Nothing

handleStop :: forall (api :: *) m. (HasResolver m api, HasObjectDefinition api) =>
  ServerOptions m api -> ConnectionContext -> OperationMessage -> IO ()
handleStop ServerOptions{..} connCtx message = do
  opId <- case omId message of 
            Just id -> return id
            Nothing -> do
              sendErrorText connCtx Nothing "No opId given in GQL_START" Nothing
              throw (BadClientRequest "Received GQL_STOP with no operation ID")
  putStrLn $ "CANCELLING OPERATION " ++ show opId
  unsubscribe connCtx opId 

mbToEither :: Maybe b -> a -> Either a b
mbToEither Nothing ex = Left ex
mbToEither (Just v) ex = Right v

toVarVals :: Maybe Aeson.Value -> Either Text GraphQL.VariableValues
toVarVals Nothing = Right Map.empty
toVarVals (Just (Aeson.Object hm)) = Right
                                       $ Map.fromList 
                                       $ catMaybes 
                                       $ fromAesonPair <$> HML.toList hm
toVarVals (Just v) = Left ("'variables' field in operation message should be a JSON \
                            \object, got " <> pack (show v) <> " instead")
fromAesonPair :: (Text, Aeson.Value) -> Maybe (Variable, Value)
fromAesonPair (var, o@(Aeson.Object hm)) = (AST.Variable (Name var),) <$> fromAesonValue o
fromAesonValue :: Aeson.Value -> Maybe Value
fromAesonValue (Aeson.Object o) = Just 
                                  $ toValue 
                                  $ objectFromList 
                                  $ tossNothings 
                                  $ bimap Name fromAesonValue <$> HML.toList o
  where
    bimap f g (a, b) = (f a, g b)
    mkName = AST.Variable . Name
    tossNothings :: [(a, Maybe b)] -> [(a, b)]
    tossNothings = mapMaybe sequenceA

