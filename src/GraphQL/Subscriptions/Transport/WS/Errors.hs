
module GraphQL.Subscriptions.Transport.WS.Errors where

import Data.Text (Text)
import Control.Exception

data GQLException = BadClientRequest Text | ServerConfigException Text
    deriving Show

instance Exception GQLException
