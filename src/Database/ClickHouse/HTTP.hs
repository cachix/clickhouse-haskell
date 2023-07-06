module Database.ClickHouse.HTTP
  ( -- * Connection
    Types.HttpConnectionInfo (..),
    defaultConnectionInfo,
    HttpConnection,

    -- * Open a new connection
    connect,

    -- * Interact with ClickHouse
    ping,
    query,
    queryJson,

    -- * Helpers
    extractData,
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString.Lazy qualified as LazyBytes
import Data.Either.Extra (maybeToEither)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encode
import Database.ClickHouse.HTTP.Internal.UrlBuilder qualified as UrlBuilder
import Database.ClickHouse.HTTP.Types as Types
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS qualified as HTTPS

defaultConnectionInfo :: HttpConnectionInfo
defaultConnectionInfo =
  HttpConnectionInfo
    { secure = False,
      host = "localhost",
      port = 8123,
      username = "default",
      password = "",
      database = Nothing
    }

data HttpConnection = HttpConnection
  { manager :: HTTP.Manager,
    connectionInfo :: HttpConnectionInfo
  }

connect :: HttpConnectionInfo -> IO HttpConnection
connect connectionInfo@HttpConnectionInfo {..} = do
  manager <-
    if secure
      then HTTPS.newTlsManager
      else HTTP.newManager HTTP.defaultManagerSettings

  pure HttpConnection {..}

-- Client

ping :: HttpConnection -> IO LazyBytes.ByteString
ping = runCommand Ping

query :: Text -> HttpConnection -> IO LazyBytes.ByteString
query = runCommand . Query Default

queryJson :: (Aeson.FromJSON a) => Text -> HttpConnection -> IO (Either Text a)
queryJson q conn = do
  response <- runCommand (Query Json q) conn
  return $ do
    dat <- extractData response
    case Aeson.fromJSON dat of
      Aeson.Error err -> Left (Text.pack err)
      Aeson.Success res -> Right res

runCommand :: Command -> HttpConnection -> IO LazyBytes.ByteString
runCommand cmd conn = do
  let url = UrlBuilder.newUrl conn.connectionInfo cmd
      headers = authHeaders

  initialRequest <- HTTP.parseRequest (Text.unpack url)
  let request = initialRequest {HTTP.requestHeaders = headers}

  response <- HTTP.httpLbs request conn.manager
  return $ HTTP.responseBody response
  where
    authHeaders =
      [ ("X-ClickHouse-User", Text.Encode.encodeUtf8 $ conn.connectionInfo.username),
        ("X-ClickHouse-Key", Text.Encode.encodeUtf8 $ conn.connectionInfo.password)
      ]

-- JSON helpers

extractData :: LazyBytes.ByteString -> Either Text Aeson.Value
extractData bs =
  case Aeson.eitherDecode bs of
    Right (Aeson.Object obj) ->
      maybeToEither "Missing data field" (KM.lookup "data" obj)
    Right resp ->
      Left $ "JSON response is not an object: " <> Text.pack (show resp)
    Left err ->
      Left $ "Could not parse ClickHouse JSON response: " <> Text.pack err
