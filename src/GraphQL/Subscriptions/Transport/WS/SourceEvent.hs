
module GraphQL.Subscriptions.Transport.WS.SourceEvent where
-- | SourceEvent
-- A 'SourceEvent' is an event on the "sourceStream". Each event triggers a GraphQL
-- execution, resulting possibly in a DATA event over the subscription transport.
--
-- See the spec:
-- http://facebook.github.io/graphql/June2018/#sec-Source-Stream
--
import Data.Text (Text)
import qualified GraphQL.Internal.Syntax.AST as AST
import Data.Int (Int32)

type SourceEvent = AST.Value

class ToSourceEvent a where
  toSourceEvent :: a -> AST.Value

instance ToSourceEvent Int32 where
  toSourceEvent = AST.ValueInt 
instance ToSourceEvent Double where
  toSourceEvent = AST.ValueFloat 
instance ToSourceEvent Bool where
  toSourceEvent = AST.ValueBoolean 
instance ToSourceEvent Text where
  toSourceEvent t = AST.ValueString (AST.StringValue t)
instance (ToSourceEvent a) => ToSourceEvent [a] where
  toSourceEvent = AST.ValueList . AST.ListValue . fmap toSourceEvent
-- instance ToSourceEvent Aeson.Value where
--   toSourceEvent t = undefined --TODO
