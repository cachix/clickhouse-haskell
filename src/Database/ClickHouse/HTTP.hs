module Database.ClickHouse.HTTP
  ( -- * Connection
    HttpConnectionInfo (..),
    defaultConnectionInfo,
    HttpConnection,

    -- * Open a new connection
    connect,

    -- * Interact with ClickHouse
    ping,
    query,
    queryJson,
  )
where

import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as Bytes
import Data.ByteString.Lazy qualified as LazyBytes
import Data.Either.Extra (maybeToEither)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encode
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS qualified as HTTPS
import Network.URI.Encode qualified as Network.Encode

data HttpConnectionInfo = HttpConnectionInfo
  { scheme :: Text,
    host :: Text,
    port :: Int,
    username :: Text,
    password :: Text,
    database :: Maybe Text
  }

defaultConnectionInfo :: HttpConnectionInfo
defaultConnectionInfo =
  HttpConnectionInfo
    { scheme = "http",
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
  manager <- case scheme of
    "http" -> HTTP.newManager HTTP.defaultManagerSettings
    "https" -> HTTPS.newTlsManager
    unknownScheme -> error $ "Unsupported scheme: " <> Text.unpack unknownScheme

  pure HttpConnection {..}

-- Client

data Format = None | Json

instance Show Format where
  show None = ""
  show Json = "JSON"

data Command = Ping | Query Format Text

ping :: HttpConnection -> IO LazyBytes.ByteString
ping = runCommand Ping

query ::
  Text ->
  HttpConnection ->
  IO LazyBytes.ByteString
query = runCommand . Query None

queryJson :: (Aeson.FromJSON a) => Text -> HttpConnection -> IO (Either String a)
queryJson q conn = do
  response <- runCommand (Query Json q) conn
  return $ do
    dat <- extractData response
    case Aeson.fromJSON dat of
      Aeson.Error err -> Left err
      Aeson.Success res -> Right res

runCommand :: Command -> HttpConnection -> IO LazyBytes.ByteString
runCommand cmd conn = do
  let url = newUrl conn.connectionInfo cmd
      headers = authHeaders

  initialRequest <- HTTP.parseRequest url
  let request = initialRequest {HTTP.requestHeaders = headers}

  response <- HTTP.httpLbs request conn.manager
  return $ HTTP.responseBody response
  where
    authHeaders =
      [ ("X-ClickHouse-User", Text.Encode.encodeUtf8 $ conn.connectionInfo.username),
        ("X-ClickHouse-Key", Text.Encode.encodeUtf8 $ conn.connectionInfo.password)
      ]

newUrl :: HttpConnectionInfo -> Command -> String
newUrl conn cmd =
  let (path, params) =
        case cmd of
          Ping -> ("ping", "")
          Query format queryText -> ("", "query=" <> newQuery format queryText)

      newQuery format queryText =
        Network.Encode.encode (Text.unpack queryText)
          ++ ( case format of
                 None -> ""
                 Json -> " FORMAT " <> show format
             )
   in concat
        [ Text.unpack conn.scheme,
          "://",
          Text.unpack conn.host,
          ":",
          show conn.port,
          "/",
          path,
          if null params
            then ""
            else "?" <> params
        ]

extractData :: LazyBytes.ByteString -> Either String Aeson.Value
extractData bs =
  case Aeson.decode bs of
    Just (Aeson.Object json) ->
      maybeToEither "No data field" $ KM.lookup "data" json
    _ -> Left "Could not parse ClickHouse JSON response"
