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
import Data.List (intersperse)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encode
import Data.Text.Lazy qualified as Text.Lazy
import Data.Text.Lazy.Builder qualified as Builder
import Network.HTTP.Client qualified as HTTP
import Network.HTTP.Client.TLS qualified as HTTPS
import Network.URI.Encode qualified as Network.Encode

data HttpConnectionInfo = HttpConnectionInfo
  { secure :: Bool,
    host :: Text,
    port :: Int,
    username :: Text,
    password :: Text,
    database :: Maybe Text
  }

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

-- TODO: It's questionable whether this should be part of the API.
data Format = Default | Json

instance Show Format where
  show Default = ""
  show Json = "JSON"

data Command = Ping | Query Format Text

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
  let url = newUrl conn.connectionInfo cmd
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

-- URL builders

newUrl :: HttpConnectionInfo -> Command -> Text
newUrl conn cmd =
  let databaseParam = queryParamFromMaybe "database" conn.database
      (path, queryParams) = buildCommand cmd
   in Text.Lazy.toStrict $
        Builder.toLazyText $
          mconcat
            [ Builder.fromText (if conn.secure then "https://" else "http://"),
              Builder.fromText conn.host,
              Builder.singleton ':',
              Builder.fromString (show conn.port),
              Builder.singleton '/',
              Builder.fromText path,
              if null queryParams
                then mempty
                else
                  Builder.singleton '?'
                    <> renderQueryParams (buildQueryParams (databaseParam : queryParams))
            ]

type QueryParam = (Text, Text)

buildCommand :: Command -> (Text, [QueryParam])
buildCommand cmd =
  case cmd of
    Ping ->
      ("ping", [])
    Query format queryText ->
      let finalQuery =
            queryText <> case format of
              Default -> ""
              Json -> " FORMAT " <> Text.pack (show format)
       in ("", [("query", Network.Encode.encodeText finalQuery)])

queryParamFromMaybe :: Text -> Maybe Text -> QueryParam
queryParamFromMaybe key = maybe mempty (key,)

buildQueryParams :: [QueryParam] -> [QueryParam]
buildQueryParams = filter (not . Text.null . fst)

renderQueryParams :: [QueryParam] -> Builder.Builder
renderQueryParams =
  mconcat . intersperse (Builder.singleton '&') . fmap renderQueryParam

renderQueryParam :: QueryParam -> Builder.Builder
renderQueryParam (key, value) =
  Builder.fromText key <> Builder.singleton '=' <> Builder.fromText value
