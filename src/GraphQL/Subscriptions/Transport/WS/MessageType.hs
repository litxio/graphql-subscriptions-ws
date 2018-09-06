
module GraphQL.Subscriptions.Transport.WS.MessageType where

-- | See message-types.ts in apollographql/subscriptions-transport-ws
-- Note that we do not support the deprecated message types for the old protocol.

import Data.Text

data MessageType = 
  GQL_CONNECTION_INIT-- Client -> Server
  | GQL_CONNECTION_ACK -- Server -> Client
  | GQL_CONNECTION_ERROR -- Server -> Client
  -- NOTE: The keep alive message type does not follow the standard due to connection optimizations
  | GQL_CONNECTION_KEEP_ALIVE -- Server -> Client
  | GQL_CONNECTION_TERMINATE -- Client -> Server
  | GQL_START -- Client -> Server
  | GQL_DATA -- Server -> Client
  | GQL_ERROR -- Server -> Client
  | GQL_COMPLETE -- Server -> Client
  | GQL_STOP -- Client -> Server
  deriving (Eq, Show)

msgTypeToText :: MessageType -> Text
msgTypeToText GQL_CONNECTION_INIT = "connection_init"
msgTypeToText GQL_CONNECTION_ACK = "connection_ack"
msgTypeToText GQL_CONNECTION_ERROR = "connection_error"
msgTypeToText GQL_CONNECTION_KEEP_ALIVE = "ka"
msgTypeToText GQL_CONNECTION_TERMINATE = "connection_terminate"
msgTypeToText GQL_START = "start"
msgTypeToText GQL_DATA = "data"
msgTypeToText GQL_ERROR = "error"
msgTypeToText GQL_COMPLETE = "complete"
msgTypeToText GQL_STOP = "stop"

parseMsgType :: Text -> Maybe MessageType
parseMsgType t = case t of
                  "connection_init" -> Just GQL_CONNECTION_INIT
                  "connection_ack" -> Just GQL_CONNECTION_ACK
                  "connection_error" -> Just GQL_CONNECTION_ERROR
                  "ka" -> Just GQL_CONNECTION_KEEP_ALIVE
                  "connection_terminate" -> Just GQL_CONNECTION_TERMINATE
                  "start" -> Just GQL_START
                  "data" -> Just GQL_DATA
                  "error" -> Just GQL_ERROR
                  "complete" -> Just GQL_COMPLETE
                  "stop" -> Just GQL_STOP
                  otherwise -> Nothing
