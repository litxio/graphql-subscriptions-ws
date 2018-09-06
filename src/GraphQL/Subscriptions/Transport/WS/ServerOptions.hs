module GraphQL.Subscriptions.Transport.WS.ServerOptions where

import Data.Text
import GraphQL
import GraphQL.Resolver
import GraphQL.Internal.Validation
import GraphQL.Internal.Name
import qualified GraphQL.Internal.Syntax.AST as AST
import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson as Aeson
import Control.Concurrent.Chan.Unagi (InChan, OutChan)

import GraphQL.Subscriptions.Transport.WS.Internal

data ServerOptions m api = ServerOptions {
  -- rootValue?: any,
  rootHandler :: Handler m api,
  schema :: GraphQL.Schema,
  execute :: ExecuteFunction m api,
  sourceEventGenerators :: HM.HashMap Text (InChan AST.Value -> IO ()),
  -- validationRules?: Array<(context: ValidationContext) => any>,

  -- onOperation?: Function,   -- TODO
  -- onOperationComplete?: Function,
  onConnect :: Maybe (Maybe Aeson.Value -> ConnectionContext -> IO Bool),
  -- onDisconnect?: Function,  --TODO
  keepAlive :: Maybe Int
}

type ExecuteFunction m (api :: *) = Handler m api
                                    -> QueryDocument VariableValue
                                    -> Maybe Name
                                    -> VariableValues
                                    -> IO Response

