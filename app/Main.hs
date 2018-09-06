
module Main where
-- First, the usual graphql-api Objects
--

import Data.Text (Text)
import GraphQL
import GraphQL.API
import GraphQL.Resolver
import Data.Int (Int32)
import Control.Monad (forever)
import qualified Data.HashMap.Strict as HM
import Control.Concurrent.Chan.Unagi (InChan, OutChan, writeChan)
import Control.Concurrent (threadDelay)
import qualified Network.WebSockets as WS

import Data.Time.Clock.POSIX
import Data.Time
import Data.Time.Clock


import GraphQL.Subscriptions.Transport.WS.SourceEvent
import GraphQL.Subscriptions.Transport.WS.ServerOptions
import GraphQL.Subscriptions.Transport.WS.Server (wsServer)

type Now = Object "Now" '[]
  '[
  Field "date" Date,
  Field "time" Time
   ]

type Date = Object "Date" '[]
  '[
  Field "year" Int32,
  Field "month" Int32,
  Field "day" Int32
   ]

type Time = Object "Time" '[]
  '[
  Field "hour" Int32,
  Field "minute" Int32,
  Field "second" Int32
  ]

-- Next, the root object.  "now" is a subcription operation.  The first argument to a 
-- subcription operation needs to be named "sourceEvent" and will be passed to the
-- handler on each event that comes through the source stream.

type GQLRoot = Object "GQLRoot" '[]
  '[
  Argument "sourceEvent" Int32 :> Field "now" Now
   ]

gqlRoot :: Handler IO GQLRoot
gqlRoot = pure $ handleNow
 
-- | The argument to the subscription handler is a source event created by the
-- sourceEventGenerator here, it is the current POSIX time.  
handleNow :: Int32 -> Handler IO Now
handleNow sourceEvent = do
  pure $ date :<> time
  where 
    UTCTime day diffTime = posixSecondsToUTCTime (fromIntegral sourceEvent)
    (year, month, dayOfMonth) = toGregorian day
    TimeOfDay hour minute second = timeToTimeOfDay diffTime
 
    date :: Handler IO Date
    date = pure $ pure (fromIntegral year)
                          :<> pure (fromIntegral month) 
                          :<> pure (fromIntegral dayOfMonth)
    time :: Handler IO Time
    time = pure $ pure (fromIntegral hour) 
                          :<> pure (fromIntegral minute) 
                          :<> pure (round second)

-- Finally the source event generator.  For each subscription, there must be an entry mapping
-- the operation name to the function that places events on the source stream/channel.
mySourceStreams :: HM.HashMap Text (InChan SourceEvent -> IO ())
mySourceStreams = HM.fromList [ ("now" :: Text, nowSource) ]

nowSource :: InChan SourceEvent -> IO ()
nowSource outChan = do
  forever $ do
    threadDelay 1000000
    time <- round <$> getPOSIXTime :: IO Int32
    writeChan outChan $ toSourceEvent time


defaultServerOpts :: forall api. (HasResolver IO api, HasObjectDefinition api) 
            => Handler IO api -> ServerOptions IO api
defaultServerOpts rootHandler = ServerOptions {
  rootHandler = rootHandler,
  schema = schema,
  execute = \hdr qDoc opName varVals -> executeQuery @api hdr qDoc opName varVals,
  sourceEventGenerators = mySourceStreams,
  onConnect = Nothing,
  keepAlive = Nothing }
    where schema = case makeSchema @api of 
                     Left queryError -> error ("Could not build schema:" ++ show queryError)
                     Right s         -> s

main :: IO ()
main = WS.runServer "127.0.0.1" 9160 $ wsServer (defaultServerOpts @GQLRoot gqlRoot)
